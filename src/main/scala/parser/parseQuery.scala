package parser

import tokenizer.{SpannedToken, Token}
import parser.AstNode.{Atom, Bool, Number, Query, Value, Var}

/** Determina si un identificador representa una variable (empieza con mayúscula o `_`). */
private def isVarName(name: String): Boolean =
  name.headOption.exists(c => c.isUpper || c == '_')

/**
 * Parsea un valor simple (número, string, booleano, variable o átomo)
 * desde el inicio de la lista de tokens.
 */
private[parser] def parseValue(
  tokens: List[SpannedToken]
): Either[ParserError, (Value, List[SpannedToken])] =
  tokens match {
    case Nil => Left(ParserError.UnexpectedEOF)
    case SpannedToken(Token.Number(n), _) :: rest =>
      Right((Value(Number(n)), rest))
    case SpannedToken(Token.String(s), _) :: rest =>
      Right((Value(Atom(s)), rest))
    case SpannedToken(Token.Identifier("true"), _) :: rest =>
      Right((Value(Bool(true)), rest))
    case SpannedToken(Token.Identifier("false"), _) :: rest =>
      Right((Value(Bool(false)), rest))
    case SpannedToken(Token.Identifier(name), _) :: rest if isVarName(name) =>
      Right((Value(Var(name)), rest))
    case SpannedToken(Token.Identifier(name), _) :: rest =>
      Right((Value(Atom(name)), rest))
    case SpannedToken(token, span) :: _ =>
      Left(ParserError.UnexpectedToken(token, span))
  }

def parseQuery(tokens: List[SpannedToken]): Either[ParserError, Query] = {
  // TODO: Implementar
  throw new NotImplementedError("prolog.tokenize") // Remover cuando se implemente
}
