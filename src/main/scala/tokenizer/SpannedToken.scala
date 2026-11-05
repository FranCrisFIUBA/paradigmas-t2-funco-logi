package tokenizer

import common.Span

/**
 * Token + tramo en el archivo para que el parser pueda notificar donde están los errores.
 *
 * @param token Token
 * @param span Tramo que abarca el token en el archivo.
 */
case class SpannedToken(token: Token, span: Span)
