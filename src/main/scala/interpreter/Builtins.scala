package interpreter

import parser.AstNode.{Atom, Bool, Call, Number, Value, Var}

/**
 * Operaciones built-in
 *
 * Reciben argumentos ya evaluados (los built-ins anidados se resuelven antes,
 * ver `Interpreter.State.eval`) y devuelven su resultado,
 * o None si los argumentos no son válidos para la operación.
 */
object Builtins {
  private val arities: Map[String, Int] = Map(
    "add" -> 2,
    "sub" -> 2,
    "mult" -> 2,
    "eq" -> 2,
    "gt" -> 2,
    "eqStr" -> 2,
    "len" -> 1,
    "not" -> 1
  )

  /** Built-ins booleanos que pueden usarse como metas */
  private val booleanOps: Set[String] = Set("eq", "gt", "eqStr", "not")

  def isBuiltin(name: String): Boolean = arities.contains(name)

  def hasValidArity(name: String, arity: Int): Boolean = arities.get(name).contains(arity)

  def returnsBoolean(name: String): Boolean = booleanOps.contains(name)

  def eval(name: String, args: List[Value]): Option[Value] = (name, args) match {
    case ("add", List(Value(Number(a)), Value(Number(b)))) => Some(Value(Number(a + b)))
    case ("sub", List(Value(Number(a)), Value(Number(b)))) => Some(Value(Number(a - b)))
    case ("mult", List(Value(Number(a)), Value(Number(b)))) => Some(Value(Number(a * b)))
    case ("eq", List(a, b)) if isGround(a) && isGround(b) => Some(Value(Bool(a == b)))
    case ("gt", List(Value(Number(a)), Value(Number(b)))) => Some(Value(Bool(a > b)))
    case ("eqStr", List(Value(Atom(a)), Value(Atom(b)))) => Some(Value(Bool(a == b)))
    case ("len", List(Value(Atom(a)))) => Some(Value(Number(a.length)))
    case ("not", List(Value(Bool(b)))) => Some(Value(Bool(!b)))
    case _ => None
  }

  private def isGround(value: Value): Boolean = value match {
    case Value(Var(_)) => false
    case Value(Call(_, args)) => args.forall(isGround)
    case _ => true
  }
}
