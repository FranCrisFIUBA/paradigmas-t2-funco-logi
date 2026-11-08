package tokenizer

/**
 * Función encargada de tokenizar y realizar el análisis léxico de
 * la base de conocimientos.
 *
 * @param lines Base de conocimiento como una lista de líneas
 * @return Lista de tokens para el parser o error
 */
def tokenizeProlog(lines: List[String]): Either[TokenizerError, List[SpannedToken]] = {
  @annotation.tailrec
  def loop(numbered: List[(String, Int)], acc: List[SpannedToken]): Either[TokenizerError, List[SpannedToken]] =
    numbered match {
      case Nil => Right(acc)
      case (line, _) :: rest if isIgnored(line) => loop(rest, acc)
      case (line, index) :: rest =>
        tokenizeLine(line, index + 1) match {
          case Left(error) => Left(error)
          case Right(tokens) => loop(rest, acc ++ tokens)
        }
    }

  loop(lines.zipWithIndex, Nil)
}

/** Líneas que no se tokenizan: vacías o comentarios ``%%``. */
private def isIgnored(line: String): Boolean =
  line.trim.isEmpty || line.trim.startsWith("%%")
