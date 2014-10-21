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
  lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in", "fst", "snd")

  // 0 => 0, n => Succ(n-1)
  def convertNumeric(n: Int): Term = if (n <= 0) Zero else Succ(convertNumeric(n - 1))

  // let x: T = t1 in t2        =>      (\x:T.t2) t1
  def convertLet(x: String, typ: Type, t1: Term, t2: Term) = App(Abs(Var(x), typ, t2), t1)

  /**
   * Term     ::= SimpleTerm { SimpleTerm }
   */
  def Term: Parser[Term] = positioned(
    rep1(SimpleTerm) ^^ { case ts => ts.reduceLeft { App(_, _) } }
      | failure("illegal start of term"))

  /**
   * SimpleTerm ::=  "true"
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
      | "\\" ~> ident ~ (":" ~> Type) ~ ("." ~> Term) ^^ { case x ~ typ ~ body => Abs(Var(x), typ, body) }
      | "(" ~> Term <~ ")"
      | ("let" ~> ident) ~ (":" ~> Type) ~ ("=" ~> Term) ~ ("in" ~> Term) ^^ { case x ~ typ ~ t1 ~ t2 => convertLet(x, typ, t1, t2) }
      | ("{" ~> Term <~ ",") ~ (Term <~ "}") ^^ { case p1 ~ p2 => Pair(p1, p2) }
      | "fst" ~> Term ^^ { case p => First(p) }
      | "snd" ~> Term ^^ { case p => Second(p) }
      | failure("illegal start of simple term"))

  /**
   * Type       ::= SimpleType [ "->" Type ]
   */
  def Type: Parser[Type] = positioned(
    SimpleType ~ opt("->" ~> Type) ^^ {
      case st ~ None => st
      case st ~ Some(t) => Function(st, t)
    }
      | failure("illegal start of type"))
  /**
   * SimpleType	::=
   *              "Bool"
   * 			| "Nat"
   *      		| "(" Type ")"
   *         	| T "*" T
   */
  def SimpleType: Parser[Type] = positioned(
    "Bool" ^^^ Bool
      | "Nat" ^^^ Nat
      | "(" ~> Type <~ ")"
      | Type ~ ("*" ~> Type) ^^ { case t1 ~ t2 => Product(t1, t2) }
      | failure("illegal start of type"))

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
    case Zero => true
    case Succ(t) if isNumericVal(t) => true
    case _ => false
  }
  def convertToNum(nv: Term): Int = nv match {
    case Zero => 0
    case Succ(x) => convertToNum(x) + 1
    case _ => throw new Exception("convertToNum expect to see only numerical val")
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
  def typeofVar(ctx: Context, x: String): Option[Type] =
    ctx.find(p => p._1 == x) map { _._2 }
  // TODO are the rules ordered corretly?
  // TODO test this
  // TODO: Check if the error msgs are the one expected by assistants
  def typeof(ctx: Context, t: Term): Type = t match {
    case True | False => Bool
    case Zero => Nat
    case Pred(x) if typeof(ctx, x) == Nat => Nat
    case Succ(x) if typeof(ctx, x) == Nat => Nat
    case IsZero(x) if typeof(ctx, x) == Nat => Bool
    case If(t1, t2, t3) if typeof(ctx, t1) == Bool && typeof(ctx, t2) == typeof(ctx, t3) => typeof(ctx, t3)
    case Var(x) => typeofVar(ctx, x).getOrElse(throw new TypeError(t.pos, "Type of var " + x + " not found"))
    case Abs(x, typ, body) => Function(typ, typeof((x.name, typ) :: ctx, body))
    case App(t1, t2) => typeof(ctx, t1) match {
      case Function(typ11, typ12) => typeof(ctx, t2) match {
        case typ11 => typ12
        case tWeird => throw new TypeError(t.pos, "Term " + t2 + " should be of type " + typ11 + ", but is " + tWeird)
      }
      case tWeird => throw new TypeError(t.pos, "Term " + t1 + " should be a of type Function, but is " + tWeird)
    }

    // TODO: Add rules in Fig. 5

    case _ => ??? // TODO: Stuck? What to return? Error?
  }

  /**
   * Parser a given input
   */
  def parse(input: String) =
    parse_impl(new lexical.Scanner(input))

  def parse(input: java.io.InputStream) =
    parse_impl(new lexical.Scanner(StreamReader(new java.io.InputStreamReader(input))))

  private def parse_impl(tokens: lexical.Scanner) = phrase(Term)(tokens)

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the evaluation strategy used for reduction.
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

  def main(args: Array[String]): Unit = parse(System.in) match {
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
