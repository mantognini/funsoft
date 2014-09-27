package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import java.lang.Exception

/**
 * This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  def convertNumeric(x: Int): Term = if (x <= 0) Zero else Succ(convertNumeric(x - 1))

  def isNV(t: Term): Boolean = t match {
    case Zero => true
    case Succ(t) if isNV(t) => true
    case _ => false
  }

  def isBool(t: Term): Boolean = t match {
    case False => true
    case True => true
    case _ => false
  }

  def isV(t: Term): Boolean = isBool(t) || isNV(t)
  def isNormal = isV _ // for this language it's the same

  /**
   * Specifications 1/3
   *  	Write a parser that recognizes this language, using the combinator library
   *
   * Expr ::= 'true'
   * | 'false'
   * | 'if' Expr 'then' Expr 'else' Expr
   * | '0'
   * | 'succ' Expr
   * | 'pred' Expr
   * | 'iszero' Expr
   */
  def Expr: Parser[Term] = (
    numericLit ^^ { case chars => convertNumeric(chars.toInt) } |
    "false" ^^ { case chars => False } |
    "true" ^^ { case chars => True } |
    "0" ^^ { case chars => Zero } |
    ("if" ~> Expr) ~ ("then" ~> Expr) ~ ("else" ~> Expr) ^^ { case cond ~ zen ~ elze => If(cond, zen, elze) } |
    "succ" ~> Expr ^^ { case e => Succ(e) } |
    "pred" ~> Expr ^^ { case e => Pred(e) } |
    "iszero" ~> Expr ^^ { case e => IsZero(e) }
    | failure("illegal start of expression"))

  /**
   * Specifications 2/3 - 3/3
   *  	Write a reduce method which performs one step of the evaluation
   *
   *   it should print each step of the small-step reduction, starting
   *   with the input term, until it reaches a value or gets stuck.
   *   If the reduction is stuck, it should print "Stuck term: "
   *   and the term that cannot be reduced any further.
   *
   *   Each step should be printed on one line
   *   Then, it should print "Big step: " and the value found by using the big-step evaluation.
   *   If the evaluation gets stuck, it should print "Stuck term: " and the guilty term.
   *   If there are syntax errors, it should not attempt any reduction,
   *   and only print the error message.
   *
   * Example 1:
   * input: if iszero pred pred 2 then if iszero 0 then true else false else false
   * output:
   *
   * If(IsZero(Pred(Pred(Succ(Succ(Zero))))),If(IsZero(Zero),True,False),False)
   * If(IsZero(Pred(Succ(Zero))),If(IsZero(Zero),True,False),False)
   * If(IsZero(Zero),If(IsZero(Zero),True,False),False)
   * If(True,If(IsZero(Zero),True,False),False)
   * If(IsZero(Zero),True,False)
   * If(True,True,False)
   * True
   * Big step: True
   *
   * Example 2:
   * input: pred succ succ succ false
   * output:
   *
   * Pred(Succ(Succ(Succ(False))))
   * Stuck term: Pred(Succ(Succ(Succ(False))))
   * Big step: Stuck term: Succ(False)
   */

  class NoRuleApplies extends Exception

  def stepRed(t: Term): Term = t match {
    case If(True, t2, t3) => t2
    case If(False, t2, t3) => t3
    case If(t1, t2, t3) => If(stepRed(t1), t2, t3)
    case Succ(t1) => Succ(stepRed(t1))
    case Pred(Zero) => Zero
    case Pred(Succ(nv)) if isNV(nv) => nv
    case Pred(t1) => Pred(stepRed(t1))
    case IsZero(Zero) => True
    case IsZero(Succ(nv)) if isNV(nv) => False
    case IsZero(t1) => IsZero(stepRed(t1))
    case _ => throw new NoRuleApplies
  }

  def smallStepRed(t: Term): Unit = {
    println(t)
    try {
      if (!isNormal(t)) smallStepRed(stepRed(t))
    } catch {
      case e: NoRuleApplies => println("Stuck term: " + t)
    }
  }

  def bigStepEvaluation(t: Term): Unit = {

    abstract class ApplyBRuleResult
    case class Value(t: Term) extends ApplyBRuleResult
    case class Stuck(t: Term) extends ApplyBRuleResult

    // Handle both B-IFTRUE and B-IFFALSE rules (DRY)
    object BIfRule {
      def unapply(t: Term) = t match {
        case If(t1, t2, t3) => (applyBRule(t1), applyBRule(t2), applyBRule(t3)) match {
          case (Value(True), (Value(t2)), _) => Some(Value(t2))
          case (Value(False), _, Value(t3)) => Some(Value(t3))
          case _ => None
        }
        case _ => None
      }
    }

    // Handle B-SUCC rule
    object BSuccRule {
      def unapply(t: Term) = t match {
        case Succ(t1) => applyBRule(t1) match {
          case Value(nv1) if isNV(nv1) => Some(Value(Succ(nv1)))
          case _ => None
        }
        case _ => None
      }
    }

    // Handle B-PREDZERO rule
    object BPredZeroRule {
      def unapply(t: Term) = t match {
        case Pred(t1) => applyBRule(t1) match {
          case Value(Zero) => Some(Value(Zero))
          case _ => None
        }
        case _ => None
      }
    }

    // Handle B-PREDSUCC rule
    object BPredSuccRule {
      def unapply(t: Term) = t match {
        case Pred(t1) => applyBRule(t1) match {
          case Value(Succ(nv1)) if isNV(nv1) => Some(Value(nv1))
          case _ => None
        }
        case _ => None
      }
    }

    // Handle B-ISZEROZERO and B-ISZEROSUCC rules (DRY)
    object BIsZeroRule {
      def unapply(t: Term) = t match {
        case IsZero(t1) => applyBRule(t1) match {
          case Value(Zero) => Some(Value(True))
          case Value(Succ(nv1)) if isNV(nv1) => Some(Value(False))
          case _ => None
        }
        case _ => None
      }
    }

    def applyBRule(t: Term): ApplyBRuleResult = t match {
      case v if isV(v) => Value(v) // B-VALUE
      case BIfRule(v) => v // B-IFTRUE + B-IFFALSE
      case BSuccRule(nv) => nv // B-SUCC
      case BPredZeroRule(z) => z // B-PREDZERO
      case BPredSuccRule(nv) => nv // B-PREDSUCC
      case BIsZeroRule(bool) => bool // B-ISZEROZERO + B-ISZEROSUCC
      case _ => Stuck(t) // Stuck because no rule apply
    }

    print("Big step: ")
    applyBRule(t) match {
      case Value(v) => print(v)
      case Stuck(t) => print("Stuck term: " + t)
    }
  }

  def main(args: Array[String]): Unit = {
    parse(System.in, System.out)
    println
  }

  def parse(string: String, output: java.io.OutputStream) = {
    parse_impl(new lexical.Scanner(string), output)
  }

  def parse(input: java.io.InputStream, output: java.io.OutputStream) = {
    parse_impl(new lexical.Scanner(StreamReader(new java.io.InputStreamReader(input))), output)
  }

  def parse_impl(tokens: lexical.Scanner, output: java.io.OutputStream) {
    Console.withOut(output) {
      phrase(Expr)(tokens) match {
        case Success(trees, _) =>
          // TODO
          print(trees)
        case e =>
          print(e)
      }
    }
  }
}
