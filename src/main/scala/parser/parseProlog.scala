package parser

import tokenizer.{SpannedToken, Token}
import parser.AstNode.{Assign, Bool, Clause, Term, Var}

/** Determina si un identificador representa una variable (empieza con mayúscula o `_`). */
private def isVarName(name: String): Boolean = {
  name.headOption.exists(c => c.isUpper || c == '_')
}

/** Parsea una lista completa de tokens convirtiéndola en una lista de cláusulas de Prolog. */
def parseProlog(tokens: List[SpannedToken]): Either[ParserError, List[Clause]] = {
  @annotation.tailrec
  def loop(
            remaining: List[SpannedToken],
            acc: List[Clause]
          ): Either[ParserError, List[Clause]] = {
    remaining match {
      case Nil => Right(acc.reverse)
      case _ =>
        parseClause(remaining) match {
          case Left(error)           => Left(error)
          case Right((clause, next)) => loop(next, clause :: acc)
        }
    }
  }
  loop(tokens, Nil)
}

/** Parsea una única cláusula de Prolog (compuesta por una cabeza, un cuerpo y un punto final). */
private def parseClause(tokens: List[SpannedToken]): Either[ParserError, (Clause, List[SpannedToken])] = {
  parseCall(tokens).flatMap { case (head, afterHead) =>
    parseBody(afterHead).flatMap { case (terms, afterBody) =>
      expectDot(afterBody).map { afterDot =>
        (Clause(head.name, head.args, terms), afterDot)
      }
    }
  }
}

/** Parsea el cuerpo de una cláusula si está presente (tras el operador de regla), o retorna una lista vacía si no hay cuerpo. */
private def parseBody(tokens: List[SpannedToken]): Either[ParserError, (List[Term], List[SpannedToken])] = {
  tokens match {
    case SpannedToken(Token.RuleSep, _) :: rest => parseGoals(rest)
    case _  => Right((Nil, tokens))
  }
}

/** Parsea una secuencia de metas (goals) (o condiciones) separadas por comas dentro del cuerpo de una cláusula. */
private def parseGoals(tokens: List[SpannedToken]): Either[ParserError, (List[Term], List[SpannedToken])] = {
  @annotation.tailrec
  def loop(
            remaining: List[SpannedToken],
            acc: List[Term]
          ): Either[ParserError, (List[Term], List[SpannedToken])] = {
    parseGoal(remaining) match {
      case Left(error) => Left(error)
      case Right((goal, after)) =>
        after match {
          case SpannedToken(Token.Comma, _) :: rest => loop(rest, goal :: acc)
          case _  => Right(((goal :: acc).reverse, after))
        }
    }
  }
  loop(tokens, Nil)
}

/** Parsea una meta individual, identificando si es una asignación, una variable, un booleano o una llamada general. */
private def parseGoal(tokens: List[SpannedToken]): Either[ParserError, (Term, List[SpannedToken])] = {
  tokens match {
    case Nil => Left(ParserError.UnexpectedEOF)
    case SpannedToken(Token.Identifier(name), _) :: SpannedToken(Token.Equals, _) :: rest
      if isVarName(name) =>
      parseValue(rest).map { case (value, next) => (Term(Assign(name, value)), next) }
    case SpannedToken(Token.Identifier(name), _) :: rest if isVarName(name) =>
      Right((Term(Var(name)), rest))
    case SpannedToken(Token.Identifier("true"), _) :: rest =>
      Right((Term(Bool(true)), rest))
    case SpannedToken(Token.Identifier("false"), _) :: rest =>
      Right((Term(Bool(false)), rest))
    case _ =>
      parseCall(tokens).map { case (call, next) => (Term(call), next) }
  }
}

/** Valida y consume el token de punto que marca el final de una sentencia o cláusula. */
private def expectDot(tokens: List[SpannedToken]): Either[ParserError, List[SpannedToken]] = {
  tokens match {
    case SpannedToken(Token.Dot, _) :: rest => Right(rest)
    case SpannedToken(token, span) :: _     => Left(ParserError.UnexpectedToken(token, span))
    case Nil                                => Left(ParserError.UnexpectedEOF)
  }
}
