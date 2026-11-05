package tokenizer

enum TokenizerError(message: String) extends Error(message) {
  /**
   * Se encontro un ``(`` en algún lugar del codigo, pero no
   * su correspondiente ``)`` antes de que finalice el analisis.
   */
  case UnclosedParen(row: Int, col: Int)
    extends TokenizerError(s"Paréntesis abierto en $row:$col, no cerrado.")

  /**
   * Se inició un string en algún lugar del codigo, pero no
   * se cerró antes de que finalice el analisis.
   */
  case UnterminatedString(row: Int, col: Int)
    extends TokenizerError(s"String iniciado en $row:$col, no terminado.")

  /**
   * El archivo finalizó inesperadamente.
   *
   * @note
   * Si corresponde, usar una de estas:
   * * ``InvalidNumber``
   * * ``UnclosedParen``
   * * ``UnterminatedString``
   */
  case UnexpectedEOF(row: Int, col: Int)
    extends TokenizerError(s"Fin del archivo inesperado $row:$col")

  /**
   * Se encontraron caracteres no esperados.
   */
  case UnexpectedCharacter(c: Char, row: Int, col: Int)
    extends TokenizerError(s"Carácter '$c' en $row:$col no esperado.")
}
