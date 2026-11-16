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

/** Valida y consume el token de punto que marca el final de una sentencia o cláusula. */
private def expectDot(tokens: List[SpannedToken]): Either[ParserError, List[SpannedToken]] = {
  tokens match {
    case SpannedToken(Token.Dot, _) :: rest => Right(rest)
    case SpannedToken(token, span) :: _     => Left(ParserError.UnexpectedToken(token, span))
    case Nil                                => Left(ParserError.UnexpectedEOF)
  }
}
