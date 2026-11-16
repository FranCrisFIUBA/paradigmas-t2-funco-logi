package parser

import tokenizer.{SpannedToken, Token}
import parser.AstNode.{Assign, Bool, Clause, Term, Var}

/** Determina si un identificador representa una variable (empieza con mayúscula o `_`). */
private def isVarName(name: String): Boolean = {
  name.headOption.exists(c => c.isUpper || c == '_')
}

def parseProlog(tokens: List[SpannedToken]): Either[ParserError, List[Clause]] = {
  // TODO: Implementar
  throw new NotImplementedError("prolog.tokenize") // Remover cuando se implemente
}

/** Parsea una meta individual, identificando si es una asignación, una variable, un booleano o una llamada general. */
private def parseGoal(tokens: List[SpannedToken]): Either[ParserError, (Term, List[SpannedToken])] = {
  tokens match {
    case Nil => Left(ParserError.UnexpectedEOF)
    case SpannedToken(Token.Identifier(name), _) :: SpannedToken(Token.Equals, _) :: rest
      if isVarName(name) =>
      parseValue(rest).map { case (value, next) => (Term(Assign(name, value)), next) }
    case SpannedToken(Token.Identifier(name), _) :: rest if isVarName(name) =>
      Right((Term(Var(name)), rest))
    case SpannedToken(Token.Identifier("true"), _) :: rest =>
      Right((Term(Bool(true)), rest))
    case SpannedToken(Token.Identifier("false"), _) :: rest =>
      Right((Term(Bool(false)), rest))
    case _ =>
      parseCall(tokens).map { case (call, next) => (Term(call), next) }
  }
}

/** Valida y consume el token de punto que marca el final de una sentencia o cláusula. */
private def expectDot(tokens: List[SpannedToken]): Either[ParserError, List[SpannedToken]] = {
  tokens match {
    case SpannedToken(Token.Dot, _) :: rest => Right(rest)
    case SpannedToken(token, span) :: _     => Left(ParserError.UnexpectedToken(token, span))
    case Nil                                => Left(ParserError.UnexpectedEOF)
  }
}
