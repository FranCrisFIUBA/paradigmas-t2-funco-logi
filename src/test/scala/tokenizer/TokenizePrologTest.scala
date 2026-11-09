package tokenizer

class TokenizePrologTest extends munit.FunSuite {

  /** Solo los tokens, sin posiciones. */
  private def tokensOf(lines: List[String]): Either[TokenizerError, List[Token]] =
    tokenizeProlog(lines).map(_.map(_.token))

  /** Las filas de las que salieron tokens, sin repetir. */
  private def rowsOf(lines: List[String]): Either[TokenizerError, List[Int]] =
    tokenizeProlog(lines).map(_.map(_.span.row).distinct)

  test("ignora los comentarios y las líneas vacías") {
    val lines = List("%% Hechos", "", "   ", "padre(juan,maria).")
    val expected = List(
      Token.Identifier("padre"), Token.LParen, Token.Identifier("juan"), Token.Comma,
      Token.Identifier("maria"), Token.RParen, Token.Dot
    )
    assertEquals(tokensOf(lines), Right(expected))
  }

  test("cada token conserva la fila real del archivo") {
    val lines = List("%% Hechos", "padre(juan,maria).", "", "madre(luisa,pedro).")
    assertEquals(rowsOf(lines), Right(List(2, 4)))
  }

  test("una regla reconoce :- y el punto final") {
    val expected = List(
      Token.Identifier("abuelo"), Token.LParen, Token.Identifier("X"), Token.Comma, Token.Identifier("Y"), Token.RParen,
      Token.RuleSep,
      Token.Identifier("padre"), Token.LParen, Token.Identifier("X"), Token.Comma, Token.Identifier("Z"), Token.RParen,
      Token.Dot
    )
    assertEquals(tokensOf(List("abuelo(X,Y):-padre(X,Z).")), Right(expected))
  }

  test("un error informa la fila de la línea donde está") {
    val lines = List("padre(juan,maria).", "%% comentario", "padre(juan,#).")
    assertEquals(tokensOf(lines), Left(TokenizerError.UnexpectedCharacter('#', 3, 12)))
  }
}
