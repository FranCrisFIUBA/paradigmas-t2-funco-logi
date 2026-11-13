package parser

import common.Span
import parser.AstNode.{Atom, Bool, Call, Number, Value, Var}
import tokenizer.{SpannedToken, Token}

class ParseQueryTest extends munit.FunSuite {

  private def spanned(tokens: Token*): List[SpannedToken] =
    tokens.toList.zipWithIndex.map { case (tok, idx) =>
      SpannedToken(tok, Span(1, idx + 1))
    }

  private def expectedValue(
    value: Value,
    rest: List[SpannedToken] = Nil
  ): Either[ParserError, (Value, List[SpannedToken])] =
    Right((value, rest))

  private def expectedCall(
    call: Call,
    rest: List[SpannedToken] = Nil
  ): Either[ParserError, (Call, List[SpannedToken])] =
    Right((call, rest))

  test("parseValue reconoce números enteros") {
    val tokens = spanned(Token.Number(42), Token.Comma)
    assertEquals(parseValue(tokens), expectedValue(Value(Number(42)), List(SpannedToken(Token.Comma, Span(1, 2)))))
  }

  test("parseValue reconoce strings como Atom sin comillas") {
    val tokens = spanned(Token.String("hola mundo"))
    assertEquals(parseValue(tokens), expectedValue(Value(Atom("hola mundo"))))
  }

  test("parseValue reconoce booleanos true y false") {
    assertEquals(parseValue(spanned(Token.Identifier("true"))), expectedValue(Value(Bool(true))))
    assertEquals(parseValue(spanned(Token.Identifier("false"))), expectedValue(Value(Bool(false))))
  }

  test("parseValue reconoce variables que empiezan con mayúscula o _") {
    assertEquals(parseValue(spanned(Token.Identifier("X"))), expectedValue(Value(Var("X"))))
    assertEquals(parseValue(spanned(Token.Identifier("P1"))), expectedValue(Value(Var("P1"))))
    assertEquals(parseValue(spanned(Token.Identifier("_aux"))), expectedValue(Value(Var("_aux"))))
  }

  test("parseValue reconoce átomos que empiezan con minúscula") {
    assertEquals(parseValue(spanned(Token.Identifier("juan"))), expectedValue(Value(Atom("juan"))))
  }

  test("parseValue devuelve UnexpectedEOF si la lista está vacía") {
    assertEquals(parseValue(Nil), Left(ParserError.UnexpectedEOF))
  }

  test("parseValue devuelve UnexpectedToken ante un símbolo inválido como valor") {
    val tokens = spanned(Token.Comma)
    assertEquals(parseValue(tokens), Left(ParserError.UnexpectedToken(Token.Comma, Span(1, 1))))
  }

  test("parseCall reconoce un predicado de aridad 0 con paréntesis vacíos") {
    val tokens = spanned(Token.Identifier("es_cebra"), Token.LParen, Token.RParen)
    assertEquals(parseCall(tokens), expectedCall(Call("es_cebra", Nil)))
  }

  test("parseCall reconoce un predicado de aridad 0 sin paréntesis") {
    val tokens = spanned(Token.Identifier("sol"))
    assertEquals(parseCall(tokens), expectedCall(Call("sol", Nil)))
  }

  test("parseCall reconoce un predicado con múltiples argumentos mixtos") {
    val tokens = spanned(
      Token.Identifier("profesor"),
      Token.LParen,
      Token.Identifier("fede"),
      Token.Comma,
      Token.String("algo3"),
      Token.Comma,
      Token.Number(2025),
      Token.Comma,
      Token.Identifier("X"),
      Token.RParen
    )
    val expected: Call = Call(
      "profesor",
      List(Value(Atom("fede")), Value(Atom("algo3")), Value(Number(2025)), Value(Var("X")))
    )
    assertEquals(parseCall(tokens), expectedCall(expected))
  }

  test("parseCall rechaza nombres de predicado con mayúscula o booleanos") {
    val upper = spanned(Token.Identifier("Padre"), Token.LParen, Token.Identifier("juan"), Token.RParen)
    assertEquals(parseCall(upper), Left(ParserError.InvalidPredicateName("Padre", Span(1, 1))))

    val bool = spanned(Token.Identifier("true"), Token.LParen, Token.RParen)
    assertEquals(parseCall(bool), Left(ParserError.InvalidPredicateName("true", Span(1, 1))))
  }

  test("parseCall devuelve error ante coma colgante, falta de coma o paréntesis sin cerrar") {
    val trailingComma = spanned(Token.Identifier("padre"), Token.LParen, Token.Identifier("juan"), Token.Comma, Token.RParen)
    assertEquals(parseCall(trailingComma), Left(ParserError.UnexpectedToken(Token.RParen, Span(1, 5))))

    val missingComma = spanned(Token.Identifier("padre"), Token.LParen, Token.Identifier("juan"), Token.Identifier("X"), Token.RParen)
    assertEquals(parseCall(missingComma), Left(ParserError.UnexpectedToken(Token.Identifier("X"), Span(1, 4))))

    val unclosed = spanned(Token.Identifier("padre"), Token.LParen, Token.Identifier("juan"))
    assertEquals(parseCall(unclosed), Left(ParserError.UnexpectedEOF))
  }
}
