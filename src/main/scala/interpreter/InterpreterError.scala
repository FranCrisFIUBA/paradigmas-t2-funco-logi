package interpreter

import common.Error

enum InterpreterError(message: String) extends Error(message) {
  /**
   * Una cláusula de la base de conocimiento usa el nombre de un built-in.
   */
  case ReservedName(name: String)
    extends InterpreterError(s"El nombre '$name' está reservado para un built-in.")

  /**
   * Un built-in se usa con una aridad incorrecta, o como meta
   * siendo que no devuelve un booleano (add, sub, mult, len).
   */
  case InvalidBuiltinUse(name: String, arity: Int)
    extends InterpreterError(s"Uso inválido del built-in '$name/$arity'.")

  /**
   * El lado derecho de una asignación es un llamado que no es un built-in,
   * por lo que no se puede evaluar a un valor.
   */
  case NotEvaluable(name: String, arity: Int)
    extends InterpreterError(s"El predicado '$name/$arity' no es evaluable como expresión.")
}
