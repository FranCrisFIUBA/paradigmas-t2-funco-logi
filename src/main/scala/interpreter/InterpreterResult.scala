package interpreter

import parser.AstNode.{Atom, Bool, Call, Number, Value, Var}

/**
 * Resultado de una consulta.
 *
 * @param result Valor de verdad de la consulta
 * @param solutions Una lista por cada solución encontrada, con
 *                  los valores de las variables de la consulta que quedaron ligadas.
 */
final case class InterpreterResult private (result: Boolean, solutions: List[List[(String, Value)]]) {
  /**
   * Formatos:
   * + `false`
   * + `true`
   * + `true [(X=a;Y=b),(X=c;Y=d)]`
   */
  override def toString: String =
    if (!result) "false"
    else if (solutions.forall(_.isEmpty)) "true"
    else solutions
      .map(_.map { case (name, value) => s"$name=${InterpreterResult.show(value)}" }.mkString("(", ";", ")"))
      .mkString("true [", ",", "]")
}

object InterpreterResult {
  val True: InterpreterResult = InterpreterResult(true, Nil)
  val False: InterpreterResult = InterpreterResult(false, Nil)

  def withSolutions(solutions: List[List[(String, Value)]]): InterpreterResult = {
    InterpreterResult(true, solutions)
  }

  private def show(value: Value): String = value.value match {
    case Var(name) => name
    case Number(n) => n.toString
    case Bool(b) => b.toString
    case Atom(a) => a
    case Call(name, args) => s"$name(${args.map(show).mkString(",")})"
  }
}
