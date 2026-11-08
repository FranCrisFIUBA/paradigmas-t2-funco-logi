package tokenizer

/**
 * Función encargada de tokenizar y realizar el análisis léxico de la consulta.
 *
 * @param line Consulta realizada
 * @return Lista de tokens para el parser o error
 */
def tokenizeQuery(line: String): Either[TokenizerError, List[SpannedToken]] =
  tokenizeLine(line, 1)
