package interpreter

enum InterpreterError {
  /**
   * Una cláusula de la base de conocimiento usa el nombre de un built-in.
   */
  case ReservedName(name: String)

  /**
   * Un built-in se usa con una aridad incorrecta, o como meta
   * siendo que no devuelve un booleano (add, sub, mult, len).
   */
  case InvalidBuiltinUse(name: String, arity: Int)

  /**
   * El lado derecho de una asignación es un llamado que no es un built-in,
   * por lo que no se puede evaluar a un valor.
   */
  case NotEvaluable(name: String, arity: Int)
}
