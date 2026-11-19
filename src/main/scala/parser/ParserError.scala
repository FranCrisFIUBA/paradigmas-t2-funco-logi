package parser

import common.{Error, Span}
import tokenizer.Token

enum ParserError(message: String) extends Error(message) {
  case UnexpectedToken(token: Token, span: Span)
    extends ParserError(s"Token '$token' inesperado en ${span.row}:${span.col}.")

  case UnexpectedEOF
    extends ParserError("Fin de entrada inesperado.")

  case InvalidPredicateName(name: String, span: Span)
    extends ParserError(s"Nombre de predicado inválido '$name' en ${span.row}:${span.col}.")
}
