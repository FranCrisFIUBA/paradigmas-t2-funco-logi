package interpreter

import parser.AstNode.{Clause, Query}

final case class Interpreter private () {
  def evalQuery(query: Query): Either[InterpreterError, InterpreterResult] = {
    // TODO: Implementar
    throw new NotImplementedError("interpreter.Interpreter.evalQuery") // Remover cuando se implemente
  }
}

object Interpreter {
  def create(clauses: List[Clause]): Either[InterpreterError, Interpreter] = {
    // TODO: Implementar
    throw new NotImplementedError("interpreter.Interpreter.create") // Remover cuando se implemente
  }
}
