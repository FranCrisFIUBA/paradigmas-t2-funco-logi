package parser

import common.Span
import parser.AstNode.{Assign, Atom, Bool, Call, Clause, Number, Term, Value, Var}
import tokenizer.{SpannedToken, Token, tokenizeProlog}

class ParsePrologTest extends munit.FunSuite {

  private def parseLines(lines: String*): Either[common.Error, List[Clause]] = {
    tokenizeProlog(lines.toList).flatMap(parseProlog)
  }

  test("parseProlog devuelve lista vacía si no hay cláusulas") {
    assertEquals(parseProlog(Nil), Right(Nil))
    assertEquals(parseLines("%% solo comentarios", ""), Right(Nil))
  }

  test("parseProlog parsea hechos de aridad 0 y aridad N") {
    val result = parseLines(
      "sol.",
      "es_mamifero().",
      "padre(juan,maria)."
    )
    val expected: List[Clause] = List(
      Clause("sol", Nil, Nil),
      Clause("es_mamifero", Nil, Nil),
      Clause("padre", List(Value(Atom("juan")), Value(Atom("maria"))), Nil)
    )
    assertEquals(result, Right(expected))
  }

  test("parseProlog parsea reglas con múltiples metas en el cuerpo") {
    val result = parseLines("abuelo(X,Y):-padre(X,Z),padre(Z,Y).")
    val expected: List[Clause] = List(
      Clause(
        "abuelo",
        List(Value(Var("X")), Value(Var("Y"))),
        List(
          Term(Call("padre", List(Value(Var("X")), Value(Var("Z"))))),
          Term(Call("padre", List(Value(Var("Z")), Value(Var("Y")))))
        )
      )
    )
    assertEquals(result, Right(expected))
  }

  test("parseProlog parsea asignaciones con built-ins anidados y metas booleanas") {
    val result = parseLines(
      "sumaYDuplica(X,Y,Z):-Z=mult(add(X,Y),2).",
      "distintos(X,Y):-not(eq(X,Y)).",
      "rumia():-false.",
      "check(X):-X."
    )
    val expected: List[Clause] = List(
      Clause(
        "sumaYDuplica",
        List(Value(Var("X")), Value(Var("Y")), Value(Var("Z"))),
        List(
          Term(
            Assign(
              "Z",
              Value(Call("mult", List(Value(Call("add", List(Value(Var("X")), Value(Var("Y"))))), Value(Number(2)))))
            )
          )
        )
      ),
      Clause(
        "distintos",
        List(Value(Var("X")), Value(Var("Y"))),
        List(Term(Call("not", List(Value(Call("eq", List(Value(Var("X")), Value(Var("Y")))))))))
      ),
      Clause("rumia", Nil, List(Term(Bool(false)))),
      Clause("check", List(Value(Var("X"))), List(Term(Var("X"))))
    )
    assertEquals(result, Right(expected))
  }

  test("parseProlog detecta falta de punto final, cuerpo vacío, coma colgante y nombre inválido") {
    assertEquals(parseLines("padre(juan,maria)"), Left(ParserError.UnexpectedEOF))
    assertEquals(parseLines("regla():-."), Left(ParserError.UnexpectedToken(Token.Dot, Span(1, 10))))
    assertEquals(parseLines("regla():-a(),."), Left(ParserError.UnexpectedToken(Token.Dot, Span(1, 14))))
    assertEquals(parseLines("Padre(juan)."), Left(ParserError.InvalidPredicateName("Padre", Span(1, 1))))
  }
}

