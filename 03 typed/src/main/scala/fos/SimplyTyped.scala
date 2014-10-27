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
    def boolean = "Bool" ^^^ Bool
    def natural = "Nat" ^^^ Nat

    positioned(function)
  }

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(pos: Position, msg: String) extends Exception(msg) {
    override def toString =
      msg + "\n" + pos.longString
  }

  /** The context is a list of variable names paired with their type. */
  type Context = Map[String, Type]

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
    case True | False => true
    case nv if isNumericVal(nv) => true
    case Abs(_, _, _) => true
    case Pair(v1, v2) if isValue(v1) && isValue(v2) => true
    case _ => false
  }

  /** Alpha-conversion **/
  def alpha(t: Term, y: Var): Term = {
    val oldName = y.name // "prefix"

    def findUsedNameWithPrefix: Set[String] = {
      def addIfPrefix(acc: Set[String], name: String): Set[String] = if (name != oldName && name.startsWith(oldName)) acc + name else acc

      def walk(cur: Term, acc: Set[String]): Set[String] = cur match {
        case Var(vn) => addIfPrefix(acc, vn)
        case Abs(Var(vn), _, t1) => walk(t1, addIfPrefix(acc, vn))
        case App(t1, t2) => walk(t2, walk(t1, acc))
      }

      walk(t, Set())
    }

    def convertToInteger(str: String): Int = try {
      str.toInt
    } catch {
      case e: java.lang.NumberFormatException => 0
    }

    // if v, v1, v2, .., vi, vj (j is not necessarily i + 1), then the selected name is v{j+1}. 
    def findFirstFreeName: String = {
      val taken = findUsedNameWithPrefix
      val indexes = taken map { i => convertToInteger(i.stripPrefix(oldName)) }
      val lastIndex = (indexes + 0) max

      oldName + (lastIndex + 1)
    }

    def rename(cur: Term, newName: String): Term = cur match {
      case Var(vn) if vn == oldName => Var(newName)
      case v @ Var(_) => v
      case l @ Abs(Var(vn), _, _) if vn == oldName => l
      case Abs(v, typ, t1) => Abs(v, typ, rename(t1, newName))
      case App(t1, t2) => App(rename(t1, newName), rename(t2, newName))
    }

    val newName = findFirstFreeName
    t match {
      case Abs(x, typ, t) => Abs(Var(newName), typ, rename(t, newName))
      case _ => throw new RuntimeException("Unexpected input term for alpha conversion")
    }
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    //   ... To complete ... 
    case _ =>
      throw NoRuleApplies(t)
  }

  /** Define what is a non-composed type **/
  def isNotComposedType(t: Type): Boolean = t match {
    case Bool | Nat => true
    case _ => false
  }

  /**
   * Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(t: Term)(implicit ctx: Context = Map()): Type = {
    def typeofVar(x: Var) = ctx.getOrElse(x.name, throw new TypeError(t.pos, "unknown var " + x.name))

    t match {
      case True | False => Bool

      case Zero => Nat

      case Pred(t) if typeof(t) == Nat => Nat

      case Succ(t) if typeof(t) == Nat => Nat

      case IsZero(t) if typeof(t) == Nat => Bool

      case If(t1, t2, t3) if typeof(t1) == Bool && typeof(t2) == typeof(t3) => typeof(t3)

      case x: Var => typeofVar(x)

      // This «\x: Bool. \x: Nat. (\y: Bool. 0) x» should fail
      // This «\x: Bool. \x: Nat. (\y: Nat . 0) x» should pass
      case Abs(x, typ, body) => Function(typ, typeof(body)(ctx + ((x.name, typ))))

      case App(t1, t2) => typeof(t1) match {
        case Function(typ11, typ12) =>
          val typ22 = typeof(t2)
          if (typ22 == typ11) typ12
          else throw TypeError(t.pos, "Term " + t2 + " should be of type " + typ11 + ", but is " + typ22)

        case typError => throw TypeError(t.pos, "Term " + t1 + " should be of type Function, but is " + typError)
      }

      case Pair(t1, t2) => Product(typeof(t1), typeof(t2))

      case First(t) => typeof(t) match {
        case Product(typ1, _) => typ1
        case typError => throw TypeError(t.pos, "Term " + t + " should be of type Product, but is " + typError)
      }

      case Second(t) => typeof(t) match {
        case Product(_, typ2) => typ2
        case typError => throw TypeError(t.pos, "Term " + t + " should be of type Product, but is " + typError)
      }

      case _ => throw TypeError(t.pos, "No type checking rules apply to " + t)
    }
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
