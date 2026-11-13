package interpreter

import parser.AstNode.{Clause, Query}

final case class Interpreter private (
                                       /**
                                        * Cláusulas agrupadas por nombre y
                                        * ordenadas por aparición
                                        */
                                       clauses: Map[String, List[Clause]]
                                     ) {
  def evalQuery(query: Query): Either[InterpreterError, InterpreterResult] = {
    // TODO: Implementar
    throw new NotImplementedError("interpreter.Interpreter.evalQuery") // Remover cuando se implemente
  }
}

object Interpreter {
  def create(clauses: List[Clause]): Either[InterpreterError, Interpreter] = {
    Right(Interpreter(
      clauses
        // groupBy ordena los elementos de las listas por orden de aparición
        .groupBy(_.name)
    ))
  }
}
