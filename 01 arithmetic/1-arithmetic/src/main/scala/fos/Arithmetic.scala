package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
 * This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  def convertNumeric(x: Int): Term = if (x <= 0) Zero else Succ(convertNumeric(x - 1))

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
    ("if" ~> Expr) ~ ("then" ~> Expr) ~ ("else" ~> Expr) ^^ { case cond ~ zen ~ elze => IfThenElse(cond, zen, elze) } |
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
  def smallStepReduction = ???
  def bigStepEvaluation = ???

  def main(args: Array[String]): Unit = {
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Expr)(tokens) match {
      case Success(trees, _) =>
        //   ... To complete ... 
        println(trees)
      case e =>
        println(e)
    }
  }
}
