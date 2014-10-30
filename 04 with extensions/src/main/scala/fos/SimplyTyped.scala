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
  lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "fix", "letrec", "in", "fst", "snd")

  // 0 => 0, n => Succ(n-1)
  def convertNumeric(n: Int): Term = if (n <= 0) Zero() else Succ(convertNumeric(n - 1))

  // let x: T = t1 in t2        =>      (\x:T.t2) t1
  def convertLet(x: String, typ: Type, t1: Term, t2: Term) = App(Abs(Var(x), typ, t2), t1)

  // letrec x: T = t1 in t2     =>      let x = fix (\x:T. t1) in t2
  def convertLetrec(x: String, typ: Type, t1: Term, t2: Term) = convertLet(x, typ, Fix(Abs(Var(x), typ, t1)), t2)

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
    "true" ^^^ True()
      | "false" ^^^ False()
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
      | "fix" ~> Term ^^ Fix
      | ("letrec" ~> ident) ~ (":" ~> Type) ~ ("=" ~> Term) ~ ("in" ~> Term) ^^ { case x ~ typ ~ t1 ~ t2 => convertLetrec(x, typ, t1, t2) }
      | failure("illegal start of simple term"))

  /**
   * Type ::= Tp [ -> Type ]    // function
   *
   * Tp   ::= Type * Type       // product
   *        | ( Type )          // parentheses
   *        | Bool              // boolean
   *        | Nat               // natural number
   *
   * Note: -> and * are right associative
   * Note: * has a higher precedence than ->
   */
  def Type: Parser[Type] = {
    def function = rep1sep(product, "->") ^^ { _.reduceRight { Function(_, _) } }
    def product = rep1sep(parentheses | boolean | natural, "*") ^^ { _.reduceRight { Product(_, _) } }
    def parentheses = "(" ~> Type <~ ")"
    def boolean = "Bool" ^^^ Bool()
    def natural = "Nat" ^^^ Nat()

    positioned(function)
  }

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occurred. */
  case class TypeError(pos: Position, msg: String) extends Exception(msg) {
    override def toString =
      msg + "\n" + pos.longString
  }

  /** The context is a list of variable names paired with their type. */
  type Context = Map[String, Type]

  /** Is the given term a numeric value? */
  def isNumericVal(t: Term): Boolean = t match {
    case Zero() => true
    case Succ(t) if isNumericVal(t) => true
    case _ => false
  }

  object NumericValue {
    def unapply(t: Term) = if (isNumericVal(t)) Some(t) else None
  }

  /** Is the given term a value? */
  def isValue(t: Term): Boolean = t match {
    case True() => true
    case False() => true
    case NumericValue(_) => true
    case Abs(_, _, _) => true
    case Pair(Value(_), Value(_)) => true
    case Fix(_) => true
    case _ => false
  }

  object Value {
    def unapply(t: Term) = if (isValue(t)) Some(t) else None
  }

  /**
   * Substitution rules:
   *
   * [x → s]true                    = true
   * [x → s]false                   = false
   * [x → s]if t1 then t2 else t3   = if [x → s]t1 then [x → s]t2 else [x → s]t3
   * [x → s]pred t                  = pred [x → s]t
   * [x → s]succ t                  = succ [x → s]t
   * [x → s]iszero t                = iszero [x → s]t
   * [x → s]y                       = s                                             if y = x
   * [x → s]y                       = y                                             if y ≠ x
   * [x → s](λy. t)                 = λy. t                                         if y = x
   * [x → s](λy. t)                 = λy. [x → s]t                                  if y ≠ x
   * [x → s](t1 t2)                 = [x → s]t1 [x → s]t2
   * [x → s]{t1, t2}                = {[x → s]t1, [x → s]t2}
   * [x → s]fst t                   = fst [x → s]t
   * [x → s]snd t                   = snd [x → s]t
   */
  def substitute(body: Term, x: Var, s: Term): Term = body match {
    case True() => True()
    case False() => False()
    case If(t1, t2, t3) => If(substitute(t1, x, s), substitute(t2, x, s), substitute(t3, x, s))
    case Pred(t) => Pred(substitute(t, x, s))
    case Succ(t) => Succ(substitute(t, x, s))
    case IsZero(t) => IsZero(substitute(t, x, s))
    case y: Var if y == x => s
    case y: Var if y != x => y
    case l @ Abs(y, _, _) if y == x => l
    case Abs(y, typ, t) if y != x => Abs(y, typ, substitute(t, x, s))
    case App(t1, t2) => App(substitute(t1, x, s), substitute(t2, x, s))
    case Pair(t1, t2) => Pair(substitute(t1, x, s), substitute(t2, x, s))
    case First(t) => First(substitute(t, x, s))
    case Second(t) => Second(substitute(t, x, s))
    case Fix(t) => Fix(substitute(t, x, s))
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    // Computation
    case If(True(), t1, t2) => t1
    case If(False(), t1, t2) => t2
    case IsZero(Zero()) => True()
    case IsZero(Succ(NumericValue(nv))) => False()
    case Pred(Zero()) => Zero()
    case Pred(Succ(NumericValue(nv))) => nv
    case App(Abs(x, typ, body), Value(v2)) => substitute(body, x, v2)
    case First(Pair(Value(v1), Value(v2))) => v1
    case Second(Pair(Value(v1), Value(v2))) => v2
    case fix @ Fix(Abs(x, typ, body)) => substitute(body, x, fix)

    // Congruence
    case If(t1, t2, t3) => If(reduce(t1), t2, t3)
    case IsZero(t) => IsZero(reduce(t))
    case Pred(t) => Pred(reduce(t))
    case Succ(t) => Succ(reduce(t))
    case App(Value(v1), t2) => App(v1, reduce(t2))
    case App(t1, t2) => App(reduce(t1), t2)
    case First(t) => First(reduce(t))
    case Second(t) => Second(reduce(t))
    case Pair(Value(v1), t2) => Pair(v1, reduce(t2))
    case Pair(t1, t2) => Pair(reduce(t1), t2)
    case Fix(t) => Fix(reduce(t))

    case _ => throw NoRuleApplies(t)
  }

  /**
   * Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(t: Term)(implicit ctx: Context = Map()): Type = t match {
    case True() => Bool()

    case False() => Bool()

    case Zero() => Nat()

    case Pred(t) if typeof(t) == Nat() => Nat()

    case Succ(t) if typeof(t) == Nat() => Nat()

    case IsZero(t) if typeof(t) == Nat() => Bool()

    case If(t1, t2, t3) if typeof(t1) == Bool() && typeof(t2) == typeof(t3) => typeof(t3)

    case Var(name) => ctx.getOrElse(name, throw TypeError(t.pos, s"unknown variable $name"))

    case Abs(x, typ, body) => Function(typ, typeof(body)(ctx + ((x.name, typ))))

    case App(t1, t2) => typeof(t1) match {
      case Function(typ11, typ12) =>
        val typ22 = typeof(t2)
        if (typ22 == typ11) typ12
        else throw TypeError(t.pos, s"parameter type mismatch: expected $typ11, found $typ22")

      case typError => throw TypeError(t.pos, s"function type expected $typError found")
    }

    case Pair(t1, t2) => Product(typeof(t1), typeof(t2))

    case First(t) => typeof(t) match {
      case Product(typ1, _) => typ1
      case typError => throw TypeError(t.pos, s"pair type expected but $typError found")
    }

    case Second(t) => typeof(t) match {
      case Product(_, typ2) => typ2
      case typError => throw TypeError(t.pos, s"pair type expected but $typError found")
    }

    case _ => throw TypeError(t.pos, "no type checking rules apply to " + t)
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
        println("typed: " + typeof(trees))
        for (t <- path(trees, reduce))
          println(t)
      } catch {
        case tperror: TypeError => println(tperror.toString)
      }
    case e =>
      println(e)
  }

}
