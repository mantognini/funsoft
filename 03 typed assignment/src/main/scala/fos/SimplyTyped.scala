package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
 * This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*")
  lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
    "pred", "iszero", "let", "in", "fst", "snd")

  def convertNumeric(x: Int): Term = if (x <= 0) Zero else Succ(convertNumeric(x - 1))
  def convertLet(x: String, T: Type, t1: Term, t2: Term) = App(Abs(Var(x), t2), t1) // TODO: What to do with T ?

  /**
   * Term     ::= SimpleTerm { SimpleTerm }
   */
  def Term: Parser[Term] = positioned(
    //   ... To complete ... 
    | failure ("illegal start of term"))

  /**
   * SimpleTerm ::= "true"
   *               | "false"
   *               | number
   *               | "succ" Term
   *               | "pred" Term
   *               | "iszero" Term
   *               | "if" Term "then" Term "else" Term
   *               | ident
   *               | "\" ident ":" Type "." Term
   *               | "(" Term ")"
   *               | "let" ident ":" Type "=" Term "in" Term
   *               | "{" Term "," Term "}"
   *               | "fst" Term
   *               | "snd" Term
   */
  def SimpleTerm: Parser[Term] = positioned(
    "true" ^^^ True
      | "false" ^^^ False
      | numericLit ^^ { case chars => convertNumeric(chars.toInt) }
      | "succ" ~> Term ^^ { case e => Succ(e) }
      | "pred" ~> Term ^^ { case e => Pred(e) }
      | "iszero" ~> Term ^^ { case e => IsZero(e) }
      | ("if" ~> Term) ~ ("then" ~> Term) ~ ("else" ~> Term) ^^ { case cond ~ zen ~ elze => If(cond, zen, elze) }
      | ident ^^ { Var(_) }
      | "\\" ~> ident ~ (":" ~> Type) ~ ("." ~> Term) ^^ { case name ~ T ~ term => Abs(Var(name), term) /* TODO: What to do with T? */ }
      | "(" ~> Term <~ ")"
      | ("let" ~> ident) ~ (":" ~> Type) ~ ("=" ~> Term) ~ ("in" ~> Term) ^^ {
        // let x: T = t1 in t2		=>		(\x:T.t2) t1
        case x ~ T ~ t1 ~ t2 => convertLet(x, T, t1, t2) // TODO
      }
      | ("{" ~> Term <~ ",") ~ (Term <~ "}") ^^ { case p1 ~ p2 => Pairr(p1, p2) }
      | "fst" ~> Term ^^ { case p => Fst(p) }
      | "snd" ~> Term ^^ { case p => snd(p) }
      | failure("illegal start of simple term"))

  /**
   * Type       ::= SimpleType [ "->" Type ]
   */
  def Type: Parser[Type] = positioned(
    //   ... To complete ... 
    | failure ("illegal start of type"))

  //   ... To complete ... 

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(pos: Position, msg: String) extends Exception(msg) {
    override def toString =
      msg + "\n" + pos.longString
  }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]

  /** Is the given term a numeric value? */
  def isNumericVal(t: Term): Boolean = t match {
    //   ... To complete ... 
    case _ => false
  }

  /** Is the given term a value? */
  def isValue(t: Term): Boolean = t match {
    //   ... To complete ... 
    case _ => false
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    //   ... To complete ... 
    case _ =>
      throw NoRuleApplies(t)
  }

  /**
   * Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type = t match {
    case True | False =>
      TypeBool
    //   ... To complete ... 
  }

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the evaluation strategy used for reduction.
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
        try {
          println("typed: " + typeof(Nil, trees))
          for (t <- path(trees, reduce))
            println(t)
        } catch {
          case tperror => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
