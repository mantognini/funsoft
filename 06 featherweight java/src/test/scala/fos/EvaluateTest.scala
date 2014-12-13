package fos

import org.scalatest._

case class EvaluateTestError(str: String) extends Exception(str)

class EvaluateTest extends WordSpec with Matchers {
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

  def addClassA(): Unit = {
    val cls = "A"
    val superCls = "Object"
    val fields: List[(String, String)] = Nil
    addClass(cls, superCls, fields, Nil)
  }
  def addClassB(): Unit = {
    val cls = "A"
    val superCls = "Object"
    val fields: List[(String, String)] = Nil
    addClass(cls, superCls, fields, Nil)
  }
  def addClassPair(): Unit = {
    val cls = "Pair"
    val superCls = "Object"
    val fields: List[(String, String)] =
      "Object" -> "fst" ::
        "Object" -> "snd" ::
        Nil

    val setfstArgs = FieldDef("Object", "newfst") :: Nil
    val setfstBody = EvaluateHelper.parseExpr("new Pair(newfst, this.snd)")
    val setfst = MethodDef("Pair", "setfst", setfstArgs, setfstBody)

    addClass(cls, superCls, fields, setfst :: Nil)
  }
}

object EvaluateHelper {
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
  def evaluate(input: String): Expr = {
    val ast = EvaluateHelper.parseExpr(input)
    val expr = Evaluate(ast)
    expr
  }
  def test(steps: List[String], info: Informer): Option[String] = {
    def testAStep(step: String, expected: Option[String]): Option[String] = {
      info(s"info: testing that `$step` evaluates to `$expected`")
      expected match {
        case Some(nextStep) => {
          try {
            val effectiveNextStep = evaluate(step)
            if (effectiveNextStep == EvaluateHelper.parseExpr(nextStep)) {
              None
            } else {
              Some(s"`$step` does not evaluate to `$nextStep` but rather to `$effectiveNextStep`")
            }
          } catch {
            case e: NoRuleApplies => Some(s"Expected `$step` to evaluate into `$nextStep` but got stuck")
            case e: Throwable => throw new EvaluationException(s"Unexpected error $e")
          }
        }
        case None => {
          try {
            val effectiveNextStep = evaluate(step)
            Some(s"`$step` evaluated to `$effectiveNextStep` but was expected to be the last evaluation step")
          } catch {
            case e: NoRuleApplies => None
            case e: Throwable => throw new EvaluationException(s"Unexpected error $e")
          }
        }
      }
    }
    def testRec(steps: List[String]): Option[String] = {
      steps match {
        case Nil => None
        case lastStep :: Nil => testAStep(lastStep, None)
        case step :: xs => testAStep(step, Some(xs.head)) match {
          case e @ Some(_) => e
          case None => testRec(xs)
        }
      }
    }
    testRec(steps)
  }
}