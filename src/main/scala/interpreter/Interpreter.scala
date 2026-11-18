package interpreter

import parser.AstNode.{Assign, Bool, Call, Clause, Query, Term, Value, Var}

final case class Interpreter private (
                                       /**
                                        * Cláusulas agrupadas por nombre y
                                        * ordenadas por aparición
                                        */
                                       clauses: Map[String, List[Clause]]
                                     ) {
  import Interpreter.State

  def evalQuery(query: Query): Either[InterpreterError, InterpreterResult] =
    Interpreter.validateGoal(query.call) match {
      case Some(error) => Left(error)
      case None => Right(evalCall(query.call))
    }

  private def evalCall(call: Call): InterpreterResult = {
    val queryVars = Interpreter.collectVars(call.args)
    val states = solve(List(Term(call)), State.empty)

    if (queryVars.isEmpty) {
      if (states.nonEmpty) InterpreterResult.True else InterpreterResult.False
    } else {
      val solutions = states
        .map(state => queryVars.flatMap(name => state.resolve(name).map(name -> _)))
        .toList
      if (solutions.isEmpty) InterpreterResult.False else InterpreterResult.withSolutions(solutions)
    }
  }

  /**
   * Resuelve la lista de metas de izquierda a derecha, devolviendo
   * un estado por cada forma de satisfacerlas (en orden de aparición).
   */
  private def solve(goals: List[Term], state: State): LazyList[State] = goals match {
    case Nil => LazyList(state)
    case Term(goal) :: rest => goal match {
      case Bool(value) =>
        if (value) solve(rest, state) else LazyList.empty
      case Var(name) =>
        state.walk(Value(Var(name))) match {
          case Value(Bool(true)) => solve(rest, state)
          case _ => LazyList.empty
        }
      case Assign(name, rhs) =>
        evalRhs(rhs, state)
          .flatMap(value => state.unify(Value(Var(name)), value))
          .map(solve(rest, _))
          .getOrElse(LazyList.empty)
      case call: Call =>
        solveCall(call, rest, state)
    }
  }

  private def solveCall(call: Call, rest: List[Term], state: State): LazyList[State] =
    // Los argumentos pueden tener built-ins anidados: se evalúan antes de usarlos
    state.evalAll(call.args) match {
      case None => LazyList.empty
      case Some(args) =>
        if (Builtins.isBuiltin(call.name)) {
          Builtins.eval(call.name, args) match {
            case Some(Value(Bool(true))) => solve(rest, state)
            case _ => LazyList.empty
          }
        } else {
          LazyList.from(clauses.getOrElse(call.name, Nil)).flatMap { clause =>
            val renamed = Interpreter.rename(clause, state.nextId)
            state.copy(nextId = state.nextId + 1)
              .unifyAll(args, renamed.params)
              .map(solve(renamed.terms ::: rest, _))
              .getOrElse(LazyList.empty)
          }
        }
    }

  private def evalRhs(rhs: Value | Call, state: State): Option[Value] = rhs match {
    case value: Value => state.eval(value)
    case call: Call => state.eval(Value(call))
  }
}

object Interpreter {
  def create(clauses: List[Clause]): Either[InterpreterError, Interpreter] =
    clauses.iterator.flatMap(validateClause).nextOption() match {
      case Some(error) => Left(error)
      case None => Right(Interpreter(
        clauses
          // groupBy ordena los elementos de las listas por orden de aparición
          .groupBy(_.name)
      ))
    }

  /**
   * Estado de una rama de la búsqueda.
   *
   * @param linkedVars Variables ligadas (pueden estar ligadas a otra variable)
   * @param nextId Próximo identificador para las variables de una cláusula
   */
  private final case class State(linkedVars: Map[String, Value], nextId: Int) {
    /** Sigue las ligaduras hasta un valor concreto o una variable libre */
    def walk(value: Value): Value = value match {
      case Value(Var(name)) => linkedVars.get(name).fold(value)(walk)
      case _ => value
    }

    /**
     * Evalúa un valor: resuelve variables y reemplaza los built-ins
     * (anidados a cualquier profundidad) por su resultado.
     * Los llamados que no son built-ins quedan como términos compuestos.
     *
     * @return `None` si algún built-in no se puede evaluar (variable libre, tipos incorrectos)
     */
    def eval(value: Value): Option[Value] = value match {
      case Value(Call(name, args)) =>
        evalAll(args).flatMap { evaluated =>
          if (Builtins.isBuiltin(name)) Builtins.eval(name, evaluated)
          else Some(Value(Call(name, evaluated)))
        }
      case Value(Var(_)) => Some(walk(value))
      case _ => Some(value)
    }

    def evalAll(values: List[Value]): Option[List[Value]] =
      values.foldRight(Option(List.empty[Value])) { (value, acc) =>
        for {
          rest <- acc
          evaluated <- eval(value)
        } yield evaluated :: rest
      }

    /**
     * @param name Nombre de la variable
     * @return Valor de la variable, o `None` si está libre
     */
    def resolve(name: String): Option[Value] = walk(Value(Var(name))) match {
      case Value(Var(_)) => None
      case value => Some(value)
    }

    def unify(a: Value, b: Value): Option[State] = (walk(a), walk(b)) match {
      case (Value(Var(n)), Value(Var(m))) if n == m => Some(this)
      case (Value(Var(n)), y) => Some(copy(linkedVars = linkedVars + (n -> y)))
      case (x, Value(Var(m))) => Some(copy(linkedVars = linkedVars + (m -> x)))
      case (Value(Call(n, as)), Value(Call(m, bs))) =>
        if (n == m) unifyAll(as, bs) else None
      case (x, y) => Option.when(x == y)(this)
    }

    def unifyAll(as: List[Value], bs: List[Value]): Option[State] =
      if (as.sizeCompare(bs) != 0) None
      else as.zip(bs).foldLeft(Option(this)) {
        case (acc, (a, b)) => acc.flatMap(_.unify(a, b))
      }
  }

  private object State {
    val empty: State = State(Map.empty, 0)
  }

  /**
   * Renombra las variables de la cláusula para que no colisionen con las de
   * otras cláusulas (ni con otras aplicaciones de la misma, por recursión).
   */
  private def rename(clause: Clause, id: Int): Clause = {
    // '#' no puede aparecer en variables del programa o de la consulta.
    val suffix = s"#$id"

    def value(v: Value): Value = v match {
      case Value(Var(name)) => Value(Var(name + suffix))
      case Value(c: Call) => Value(call(c))
      case other => other
    }

    def call(c: Call): Call = Call(c.name, c.args.map(value))

    def term(t: Term): Term = Term(t.term match {
      case Assign(name, v: Value) => Assign(name + suffix, value(v))
      case Assign(name, c: Call) => Assign(name + suffix, call(c))
      case c: Call => call(c)
      case Var(name) => Var(name + suffix)
      case b: Bool => b
    })

    Clause(clause.name, clause.params.map(value), clause.terms.map(term))
  }

  private def validateClause(clause: Clause): Option[InterpreterError] =
    if (Builtins.isBuiltin(clause.name)) Some(InterpreterError.ReservedName(clause.name))
    else validateValues(clause.params)
      .orElse(clause.terms.iterator.flatMap(t => validateTerm(t.term)).nextOption())

  private def validateTerm(term: Assign | Call | Var | Bool): Option[InterpreterError] = term match {
    case call: Call => validateGoal(call)
    case Assign(_, call: Call) => validateExpression(call)
    case Assign(_, Value(call: Call)) => validateExpression(call)
    case Assign(_, value: Value) => validateValue(value)
    case _ => None
  }

  /** Un built-in usado como meta debe tener la aridad correcta y devolver un booleano */
  private def validateGoal(call: Call): Option[InterpreterError] =
    if (Builtins.isBuiltin(call.name) &&
      (!Builtins.hasValidArity(call.name, call.args.length) || !Builtins.returnsBoolean(call.name)))
      Some(InterpreterError.InvalidBuiltinUse(call.name, call.args.length))
    else validateValues(call.args)

  /** El lado derecho de una asignación debe ser un built-in con la aridad correcta */
  private def validateExpression(call: Call): Option[InterpreterError] =
    if (!Builtins.isBuiltin(call.name)) Some(InterpreterError.NotEvaluable(call.name, call.args.length))
    else if (!Builtins.hasValidArity(call.name, call.args.length))
      Some(InterpreterError.InvalidBuiltinUse(call.name, call.args.length))
    else validateValues(call.args)

  /** Los built-ins anidados en un argumento (a cualquier profundidad) deben tener la aridad correcta */
  private def validateValue(value: Value): Option[InterpreterError] = value match {
    case Value(Call(name, args)) =>
      if (Builtins.isBuiltin(name) && !Builtins.hasValidArity(name, args.length))
        Some(InterpreterError.InvalidBuiltinUse(name, args.length))
      else validateValues(args)
    case _ => None
  }

  private def validateValues(values: List[Value]): Option[InterpreterError] =
    values.iterator.flatMap(validateValue).nextOption()

  /** Recolecta los nombres de todas las variables presentes en una lista de valores (incluyendo términos compuestos). */
  private def collectVars(values: List[Value]): List[String] = {
    values.flatMap {
      case Value(Var(name)) => List(name)
      case Value(Call(_, args)) => collectVars(args)
      case _ => Nil
    }.distinct
  }
}
