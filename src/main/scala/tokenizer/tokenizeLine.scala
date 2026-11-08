package tokenizer

import common.Span

/** Símbolos de un solo carácter y el token que les corresponde. */
private val symbols: Map[Char, Token] = Map(
  '(' -> Token.LParen,
  ')' -> Token.RParen,
  ',' -> Token.Comma,
  '=' -> Token.Equals,
  '.' -> Token.Dot
)

/** Caracteres que pueden seguir a la primera letra de un identificador. */
private def isIdentifierChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'

/**
 * Tokeniza una sola línea.
 *
 * @param line Texto de la línea
 * @param row  Número de línea, para ubicar tokens y errores
 * @return Lista de tokens o error
 */
private[tokenizer] def tokenizeLine(line: String, row: Int): Either[TokenizerError, List[SpannedToken]] = {
  @annotation.tailrec
  def loop(chars: List[Char], col: Int, acc: List[SpannedToken]): Either[TokenizerError, List[SpannedToken]] =
    chars match {
      case Nil => Right(acc.reverse)
      case c :: rest if c.isWhitespace => loop(rest, col + 1, acc)
      case c :: rest if symbols.contains(c) =>
        loop(rest, col + 1, SpannedToken(symbols(c), Span(row, col)) :: acc)
      case ':' :: '-' :: rest =>
        loop(rest, col + 2, SpannedToken(Token.RuleSep, Span(row, col)) :: acc)
      case c :: _ if c.isLetter =>
        val (name, rest) = chars.span(isIdentifierChar)
        loop(rest, col + name.length, SpannedToken(Token.Identifier(name.mkString), Span(row, col)) :: acc)
      case c :: _ if c.isDigit =>
        val (digits, rest) = chars.span(_.isDigit)
        loop(rest, col + digits.length, SpannedToken(Token.Number(digits.mkString.toInt), Span(row, col)) :: acc)
      case c :: _ => Left(TokenizerError.UnexpectedCharacter(c, row, col))
    }

  loop(line.toList, 1, Nil)
}
