package parser

import tokenizer.Token

enum ParserError(message: String) extends Error(message) {
  case UnexpectedToken(token: Token)
    extends ParserError(s"Token '${token.toString}' inesperado")
}
