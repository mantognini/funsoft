package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
 * This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  def variable = ident ^^ { Var(_) }
  def abstraction = "\\" ~> ident ~ ("." ~> Term) ^^ { case name ~ term => Abs(Var(name), term) }
  def parentheses = "(" ~> Term <~ ")"
  def vap = variable | abstraction | parentheses

  /**
   * Term     ::= AbsOrVar { AbsOrVar }
   */
  def Term: Parser[Term] = (
    rep1(vap) ^^ {
      case ts =>
        def reduce(ts: List[Term]): Term = ts match {
          case a :: Nil => a
          case x :: xs => App(reduce(xs), x)
        }
        reduce(ts.reverse)
    }
    | failure("illegal start of term"))

  case class ParseException(e: String) extends Exception

  def parse(input: String) =
    parse_impl(new lexical.Scanner(input))

  def parse(input: java.io.InputStream) =
    parse_impl(new lexical.Scanner(StreamReader(new java.io.InputStreamReader(input))))

  private def parse_impl(tokens: lexical.Scanner) = phrase(Term)(tokens)

  def parseOrDie(input: String) = parse(input) match {
    case Success(ast, _) => ast
    case Failure(e, _) => throw new ParseException(e)
    case Error(e, _) => throw new ParseException(e)
  }

  /** Term 't' does not match any reduction rule. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /**
   * Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = {
    def reduce(t: Term): Option[Term] = None // TODO implement me

    reduce(t) match {
      case Some(t) => t
      case None => throw NoRuleApplies(t)
    }
  }

  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = t match {
    //   ... To complete ... 
    case _ =>
      throw NoRuleApplies(t)
  }

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] =
    try {
      var t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case NoRuleApplies(_) =>
        Stream.cons(t, Stream.empty)
    }

  def main(args: Array[String]): Unit = {
    parse(System.in) match {
      case Success(trees, _) =>
        println("normal order: ")
        for (t <- path(trees, reduceNormalOrder))
          println(t)
        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)

      case e =>
        println(e)
    }
  }
}
