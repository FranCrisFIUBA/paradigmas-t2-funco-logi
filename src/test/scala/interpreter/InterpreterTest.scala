package interpreter

import parser.{parseProlog, parseQuery}
import tokenizer.{tokenizeProlog, tokenizeQuery}

class InterpreterTest extends munit.FunSuite {

  private def buildInterpreter(program: String*): Either[common.Error, Interpreter] = {
    for {
      tokens <- tokenizeProlog(program.toList)
      clauses <- parseProlog(tokens)
      interpreter <- Interpreter.create(clauses)
    } yield interpreter
  }

  private def runQuery(interpreter: Interpreter, queryText: String): Either[common.Error, String] = {
    for {
      tokens <- tokenizeQuery(queryText)
      query <- parseQuery(tokens)
      result <- interpreter.evalQuery(query)
    } yield result.toString
  }

  private def evalProgram(queryText: String, program: String*): Either[common.Error, String] = {
    buildInterpreter(program*).flatMap(runQuery(_, queryText))
  }

  test("backtracking respeta el orden de aparición y descarta ramas fallidas") {
    val result = evalProgram(
      "rinde_parcial(Z)",
      "rinde_parcial(X):-toma(X,Y),no_para(Y).",
      "toma(fede,colectivo).",
      "toma(fede,subte).",
      "no_para(subte)."
    )
    assertEquals(result, Right("true [(Z=fede)]"))
  }

  test("predicados con el mismo nombre y distinta aridad no colisionan") {
    val interp = buildInterpreter(
      "dato(uno).",
      "dato(uno,dos)."
    )
    assertEquals(interp.flatMap(runQuery(_, "dato(X)")), Right("true [(X=uno)]"))
    assertEquals(interp.flatMap(runQuery(_, "dato(X,Y)")), Right("true [(X=uno;Y=dos)]"))
    assertEquals(interp.flatMap(runQuery(_, "dato(X,Y,Z)")), Right("false"))
  }

  test("recursión funciona sin colisión de variables entre llamadas") {
    val interp = buildInterpreter(
      "arista(a,b).",
      "arista(b,c).",
      "arista(c,d).",
      "camino(X,Y):-arista(X,Y).",
      "camino(X,Y):-arista(X,Z),camino(Z,Y)."
    )
    assertEquals(interp.flatMap(runQuery(_, "camino(a,d)")), Right("true"))
    assertEquals(interp.flatMap(runQuery(_, "camino(d,a)")), Right("false"))
    assertEquals(interp.flatMap(runQuery(_, "camino(a,Destino)")), Right("true [(Destino=b),(Destino=c),(Destino=d)]"))
  }

  test("built-ins de strings (eqStr, len) y combinaciones anidadas con not y eq") {
    val interp = buildInterpreter(
      "mismoTexto(A,B):-eqStr(A,B).",
      "largo(S,N):-N=len(S).",
      "largoDistintoDe4(S):-not(eq(len(S),4))."
    )
    assertEquals(interp.flatMap(runQuery(_, "mismoTexto(\"hola\",\"hola\")")), Right("true"))
    assertEquals(interp.flatMap(runQuery(_, "mismoTexto(\"hola\",\"chau\")")), Right("false"))
    assertEquals(interp.flatMap(runQuery(_, "largo(\"hola mundo\",N)")), Right("true [(N=10)]"))
    assertEquals(interp.flatMap(runQuery(_, "largoDistintoDe4(\"pepe\")")), Right("false"))
    assertEquals(interp.flatMap(runQuery(_, "largoDistintoDe4(\"scala\")")), Right("true"))
  }

  test("evaluación directa de built-ins en la consulta y manejo de variables libres en eq") {
    val interp = buildInterpreter("dummy().")
    assertEquals(interp.flatMap(runQuery(_, "not(false)")), Right("true"))
    assertEquals(interp.flatMap(runQuery(_, "not(eq(len(\"pepe\"),4))")), Right("false"))
    assertEquals(interp.flatMap(runQuery(_, "eq(X,5)")), Right("false"))
  }

  test("consulta con variables dentro de términos compuestos recolecta la variable") {
    val result = evalProgram(
      "envuelve(caja(X))",
      "envuelve(caja(a)).",
      "envuelve(caja(b))."
    )
    assertEquals(result, Right("true [(X=a),(X=b)]"))
  }

  test("InterpreterError reporta nombre reservado, uso inválido de built-in y expresión no evaluable") {
    val reserved = buildInterpreter("add(X,Y).")
    assertEquals(reserved, Left(InterpreterError.ReservedName("add")))
    assertEquals(reserved.left.map(_.toString), Left("El nombre 'add' está reservado para un built-in."))

    val badArity = buildInterpreter("f(X):-gt(X).")
    assertEquals(badArity, Left(InterpreterError.InvalidBuiltinUse("gt", 1)))

    val nonBoolGoal = buildInterpreter("f(X):-add(X,1).")
    assertEquals(nonBoolGoal, Left(InterpreterError.InvalidBuiltinUse("add", 2)))

    val nonBoolQuery = buildInterpreter("dummy().").flatMap(runQuery(_, "len(\"hola\")"))
    assertEquals(nonBoolQuery, Left(InterpreterError.InvalidBuiltinUse("len", 1)))

    val notEvaluable = buildInterpreter("f(X):-Y=dummy(X).", "dummy(a).")
    assertEquals(notEvaluable, Left(InterpreterError.NotEvaluable("dummy", 1)))
  }
}

