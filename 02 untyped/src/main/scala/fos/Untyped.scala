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
  def application = vap ~ rep1(vap) ^^ { // at least two terms are needed for application
    case t ~ tx =>
      def reduce(tx: List[Term]): Term = tx match {
        case t :: Nil => t
        case t :: tx => App(t, reduce(tx))
      }
      reduce(t :: tx)
  }

  /**
   * Term     ::= AbsOrVar { AbsOrVar }
   */
  def Term: Parser[Term] = (
    variable |
    abstraction |
    application |
    parentheses |
    failure("illegal start of term"))

  //   ... To complete ... 

  /** Term 't' does not match any reduction rule. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /**
   * Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = {
    def reduce(t: Term): Option[Term] = None

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
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Term)(tokens) match {
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
