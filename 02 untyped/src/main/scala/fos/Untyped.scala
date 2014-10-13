package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.collection.immutable.Set

/**
 * This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  /**
   * Term     ::= AbsOrVar { AbsOrVar }
   */
  def Term: Parser[Term] = {
    def variable = ident ^^ { Var(_) }
    def abstraction = "\\" ~> ident ~ ("." ~> Term) ^^ { case name ~ term => Abs(Var(name), term) }
    def parentheses = "(" ~> Term <~ ")"
    def vap = variable | abstraction | parentheses

    def vapsReducer(ts: List[Term]) = ts.reduceLeft { App(_, _) }

    rep1(vap) ^^ vapsReducer | failure("illegal start of term")
  }

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

  // FV == Free Variable
  def FV(t: Term): Set[Var] = t match {
    case x @ Var(_) => Set(x)
    case Abs(x, t1) => FV(t1) - x
    case App(t1, t2) => FV(t1) ++ FV(t2)
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
    def reduce(t: Term): Option[Term] = t match {
      // No reduction left to be applied
      case Var(_) => None

      //      t1 → t1p
      // ------------------
      //  λx. t1 → λx. t1p
      case Abs(x, t1) => reduce(t1) match {
        case Some(t1p) => Some(Abs(x, t1p))
        case None => None
      }

      // Rule order: A then B then C
      // ----------
      //
      //        A                        B                     C
      //
      //     t1 → t1p                                       t2 → t2p
      // ---------------- && (λx. b) t2 → [x → t2] b && ----------------
      //  t1 t2 → t1p t2                                 t1 t2 → t1 t2p
      //
      // Because leftmost, outermost redex is always reduced first
      case App(t1, t2) => {
        def ruleA = reduce(t1) map { t1p => App(t1p, t2) }

        def ruleB = t1 match {
          case Abs(x, b) => ??? // TODO
          case _ => None
        }

        def ruleC = reduce(t2) map { t2p => App(t1, t2p) }

        ruleA orElse ruleB orElse ruleC
      }
    }

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
      val t1 = reduce(t)
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
