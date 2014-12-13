package fos

import org.scalatest._

case class EvaluateTestError(str: String) extends Exception(str)

class EvaluateTest extends WordSpec with Matchers {
  def evaluate(input: String)(implicit parser: String => Expr): Expr = {
    info(s"input: $input")
    val ast = parser(input)
    info(s"AST: $ast")
    val expr = Evaluate(ast)
    info(s"-> expr: $expr")
    expr
  }
}

object CTHelper {
  def addClass(cls: String, superCls: String, fields: List[(String, String)], methods: List[MethodDef]): Unit = {
    val fieldDefs: List[FieldDef] = fields.map(p => FieldDef(p._1, p._2))
    val superCd: ClassDef = CT.lookup(superCls).getOrElse(
      throw new EvaluateTestError(s"superclass $cls not defined in CT"))
    val superVar: List[Var] = superCd.fields.map(fd => Var(fd.name))
    val ctorAssigns: List[Assign] = superCd.fields.zip(superVar).map(p => Assign(cls, p._1.name, p._2))
    val ctor: CtrDef = CtrDef(cls, fieldDefs, superVar, ctorAssigns)
    val cd: ClassDef = ClassDef(cls, superCls, fieldDefs, ctor, methods)
    CT.add(cls, cd)
  }
}

object evaluateHelper {
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
}