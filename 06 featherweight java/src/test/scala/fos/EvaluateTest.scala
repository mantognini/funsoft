package fos

import org.scalatest._

class EvaluateTest extends WordSpec with Matchers {
  private var id = 0
  def testId() = {
    id += 1
    id
  }

  def parseExpr(input: String): Expr = {
    val parser = FJ.phrase(FJ.Expr)
    val token = new FJ.lexical.Scanner(input)
    val res = parser(token)
    res match {
      case FJ.Success(ast, _) => ast
      case FJ.Failure(msg, _) => throw new RuntimeException(s"unable to parse $input: $msg")
      case FJ.Error(msg, _) => throw new RuntimeException(s"unable to parse $input: $msg")
    }
  }

  def evaluate(input: String)(implicit parser: String => Expr): Expr = {
    info(s"input: $input")
    val ast = parser(input)
    info(s"AST: $ast")
    val expr = Evaluate(ast)
    info(s"-> expr: $expr")
    expr
  }
}