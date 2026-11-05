package parser

/**
 * En este paquete se definen los nodos del AST usados tanto en
 * el parser de la base de conocimiento, como en el de la consulta.
 */

enum AstNode {
  /**
   * Variable
   */
  case Var(name: String)

  /**
   * Numero
   */
  case Number(value: Int)

  /**
   * Booleano
   */
  case Bool(value: Boolean)

  /**
   * Constantes, strings, nombres de términos compuestos.
   *
   * @note Cuando es un string, `value` solo tiene el contenido, no las comillas.
   * @note Las constantes y nombres de términos comienzan con minúsculas.
   */
  case Atom(value: String)

  /**
   * Valores. Usados como parámetros o argumentos.
   * Las variables y atómicos son tratados como valores por conveniencia.
   */
  case Value(value: Var | Number | Bool | Atom)

  /**
   * Llamado/evaluación a una cláusula.
   *
   * Dependiendo la aridad:
   * + 0: Nombre o Nombre().
   * + 1: Nombre(param).
   * + n: Nombre(param1, param2, ..., paramN).
   */
  case Call(name: String, args: List[Value])

  /**
   * Asignación de un valor o evaluación a una variable.
   *
   * @param name  Nombre de la variable a asignarle un valor
   * @param value Valor o llamado a asignar
   */
  case Assign(name: String, value: Value | Call)

  case Term(term: Assign | Call | Var | Bool)

  /**
   * Cláusula.
   *
   * <u>Hecho</u>:
   * + Nombre.
   * + Nombre().
   * + Nombre(param).
   * + Nombre(param0, param1, ..., paramN).
   *
   * <u>Regla de inferencia</u>:
   * + Nombre :- term0, term1, ..., termN.
   * + Nombre() :- term0, term1, ..., termN.
   * + Nombre(param) :- term0, term1, ..., termN.
   * + Nombre(param0, param1, ..., paramN) :- term0, term1, ..., termN.
   *
   * @param terms Términos separados por comas (ANDs)
   */
  case Clause(name: String, params: List[Value], terms: List[Term])

  /**
   * Consulta.
   */
  case Query(query: Call)
}
