package parser

import common.Span
import parser.AstNode.{Atom, Bool, Number, Value, Var}
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
}
