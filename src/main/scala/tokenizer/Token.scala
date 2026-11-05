package tokenizer

enum Token {
  /**
   * Nombres de cláusulas y variables.
   * @param name Nombre del identificador
   */
  case Identifier(name: String)

  /**
   * Número literal (entero)
   * @param value Valor
   */
  case Number(value: Int)

  /**
   * String literal
   * @param value Cadena de caracteres (sin las comillas)
   */
  case String(value: String)

  /**
   * Carácter ``(``
   */
  case LParen

  /**
   * Carácter ``)``
   */
  case RParen

  /**
   * Carácter ``,``
   */
  case Comma

  // Los de abajo no deben formar parte de la query

  /**
   * Carácter ``.``
   */
  case Dot

  /**
   * Separador entre encabezado y contenido de una regla `:-`
   */
  case RuleSep
}
