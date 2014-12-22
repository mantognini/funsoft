package fos.helper

import fos._
import Type._

import org.scalatest._

trait Loader {
  this: WordSpec =>

  def parseClass: String => ClassDef = parse(FJ.ClsDef)

  def parseExpr: String => Expr = parse(FJ.Expr)

  def load(input: String)(implicit parser: String => Tree): Type.Class = {
    info(s"input: $input")
    val ast = parser(input)
    info(s"AST: $ast")
    val typ = typeOf(ast)(emptyContext)
    info(s"type: $typ")
    typ
  }

  def loadAll(inputs: List[String])(implicit parser: String => Tree): List[Type.Class] =
    inputs map { load(_) }

  private def parse[A](p: FJ.Parser[A])(input: String): A = {
    val parser = FJ.phrase(p)
    val token = new FJ.lexical.Scanner(input)
    val res = parser(token)
    res match {
      case FJ.Success(ast, _) => ast
      case FJ.Failure(msg, _) => fail(s"unable to parse $input: $msg")
      case FJ.Error(msg, _) => fail(s"unable to parse $input: $msg")
    }
  }
}