package tokenizer

import common.Span

class TokenizeQueryTest extends munit.FunSuite {

  /** Solo los tokens, sin posiciones, para los tests que no las verifican. */
  private def tokensOf(query: String): Either[TokenizerError, List[Token]] =
    tokenizeQuery(query).map(_.map(_.token))

  test("una consulta con átomos y variables devuelve sus tokens con fila y columna") {
    val expected = List(
      SpannedToken(Token.Identifier("padre"), Span(1, 1)),
      SpannedToken(Token.LParen, Span(1, 6)),
      SpannedToken(Token.Identifier("juan"), Span(1, 7)),
      SpannedToken(Token.Comma, Span(1, 11)),
      SpannedToken(Token.Identifier("X"), Span(1, 12)),
      SpannedToken(Token.RParen, Span(1, 13))
    )
    assertEquals(tokenizeQuery("padre(juan,X)"), Right(expected))
  }

  test("los números se convierten a enteros") {
    val expected = List(
      Token.Identifier("suma"), Token.LParen, Token.Number(5), Token.Comma,
      Token.Number(13), Token.Comma, Token.Identifier("X"), Token.RParen
    )
    assertEquals(tokensOf("suma(5,13,X)"), Right(expected))
  }

  test("un string entre comillas conserva sus espacios") {
    val expected = List(Token.Identifier("len"), Token.LParen, Token.String("hola mundo"), Token.RParen)
    assertEquals(tokensOf("len(\"hola mundo\")"), Right(expected))
  }

  test("un carácter inválido devuelve error con su fila y columna") {
    assertEquals(tokensOf("padre($)"), Left(TokenizerError.UnexpectedCharacter('$', 1, 7)))
  }

  test("un número que no entra en un Int devuelve error en lugar de una excepción") {
    assertEquals(tokensOf("edad(99999999999)"), Left(TokenizerError.InvalidNumber("99999999999", 1, 6)))
  }
}
