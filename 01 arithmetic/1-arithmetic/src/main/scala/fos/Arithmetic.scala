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

  //   ... To complete ... 

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
