package parser

import tokenizer.{SpannedToken, Token}
import parser.AstNode.{Atom, Bool, Call, Number, Query, Value, Var}

/** Determina si un identificador representa una variable (empieza con mayúscula o `_`). */
private def isVarName(name: String): Boolean =
  name.headOption.exists(c => c.isUpper || c == '_')

/** Determina si un identificador es un nombre de predicado válido (minúscula y no booleano). */
private def isPredicateName(name: String): Boolean =
  name.headOption.exists(_.isLower) && name != "true" && name != "false"

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

/** Parsea una lista no vacía de argumentos separados por coma hasta el `)` de cierre. */
private def parseArgs(
  tokens: List[SpannedToken]
): Either[ParserError, (List[Value], List[SpannedToken])] = {
  @annotation.tailrec
  def loop(
    remaining: List[SpannedToken],
    acc: List[Value]
  ): Either[ParserError, (List[Value], List[SpannedToken])] =
    parseValue(remaining) match {
      case Left(err) => Left(err)
      case Right((value, afterValue)) =>
        val nextAcc = value :: acc
        afterValue match {
          case SpannedToken(Token.Comma, _) :: afterComma =>
            loop(afterComma, nextAcc)
          case SpannedToken(Token.RParen, _) :: afterRParen =>
            Right((nextAcc.reverse, afterRParen))
          case SpannedToken(token, span) :: _ =>
            Left(ParserError.UnexpectedToken(token, span))
          case Nil =>
            Left(ParserError.UnexpectedEOF)
        }
    }

  loop(tokens, Nil)
}

/**
 * Parsea una invocación a predicado (`pred`, `pred()` o `pred(arg1, ..., argN)`)
 * desde el inicio de la lista de tokens.
 */
private[parser] def parseCall(
  tokens: List[SpannedToken]
): Either[ParserError, (Call, List[SpannedToken])] =
  tokens match {
    case Nil => Left(ParserError.UnexpectedEOF)
    case SpannedToken(Token.Identifier(name), span) :: _ if !isPredicateName(name) =>
      Left(ParserError.InvalidPredicateName(name, span))
    case SpannedToken(Token.Identifier(name), _) :: SpannedToken(Token.LParen, _) :: SpannedToken(Token.RParen, _) :: rest =>
      Right((Call(name, Nil), rest))
    case SpannedToken(Token.Identifier(name), _) :: SpannedToken(Token.LParen, _) :: afterLParen =>
      parseArgs(afterLParen).map { case (args, rest) => (Call(name, args), rest) }
    case SpannedToken(Token.Identifier(name), _) :: rest =>
      Right((Call(name, Nil), rest))
    case SpannedToken(token, span) :: _ =>
      Left(ParserError.UnexpectedToken(token, span))
  }

/**
 * Parsea una consulta completa a partir de su lista de tokens.
 * Verifica que no queden tokens sin consumir luego de la invocación principal.
 */
def parseQuery(tokens: List[SpannedToken]): Either[ParserError, Query] =
  parseCall(tokens).flatMap {
    case (call, Nil) => Right(Query(call))
    case (_, SpannedToken(token, span) :: _) =>
      Left(ParserError.UnexpectedToken(token, span))
  }
