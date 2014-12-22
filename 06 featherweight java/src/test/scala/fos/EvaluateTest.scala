package fos

import org.scalatest._

case class EvaluateTestError(str: String) extends Exception(str)

class EvaluateTest extends WordSpec with Matchers {
  val validTestCases: List[List[String]] =
    // No rules applies
    ("new Pair(new A(), new B())" :: Nil) ::
      // E-ProjNew
      ("new Pair(new A(), new B()).snd" :: "new B()" :: Nil) ::
      // E-InvkNew & E-New-Arg
      ("new Pair(new A(), new B()).setfst(new B())" :: "new Pair(new B(), new Pair(new A(), new B()).snd)" :: "new Pair(new B(), new B())" :: Nil) ::
      // E-CastNew
      ("(Pair) new Pair(new A(), new B())" :: "new Pair(new A(), new B())" :: Nil) ::
      // E-Field & E-Cast (& E-CastNew & E-ProjNew)
      ("((Pair) new Pair(new Pair(new A(), new B()), new A()).fst).snd" :: "((Pair) new Pair(new A(), new B())).snd" :: "new Pair(new A(), new B()).snd" :: "new B()" :: Nil) ::
      // E-Invk-Recv (& E-Cast & E-ProjNew & E-CastNew & E-InvkNew & E-New-Arg)
      ("((Pair) new Pair(new A(), new Pair(new A(), new B())).snd).setfst(new B())" :: "((Pair) new Pair(new A(), new B())).setfst(new B())" :: "new Pair(new A(), new B()).setfst(new B())" :: "new Pair(new B(), new Pair(new A(), new B()).snd)" :: "new Pair(new B(), new B())" :: Nil) ::
      // E-Invk-Arg (& E-CastNew & E-InvkNew E-New-Arg & E-ProjNew)
      ("new Pair(new A(), new B()).setfst((A) new A())" :: "new Pair(new A(), new B()).setfst(new A())" :: "new Pair(new A(), new Pair(new A(), new B()).snd)" :: "new Pair(new A(), new B())" :: Nil) ::
      Nil

  val stuckCases: List[String] =
    // Downcast is well-typed but stuck
    "(P1) new P0()" ::
      // Stupid cast is well-typed (with a warning) but stuck
      "(A) new P0()" ::
      Nil

  CTHelper.addClassA()
  CTHelper.addClassB()
  CTHelper.addClassPair()
  CTHelper.addClassP0()
  CTHelper.addClassP1()
  CTHelper.addClassP2()

  "The Evaluator" should {
    s"evaluate expressions" in {
      validTestCases.map(EvaluateHelper.test(_, info) should be(None))
      stuckCases.map(x => EvaluateHelper.test(x :: Nil, info) should be(None))
    }
  }
}

object CTHelper {
  def addClassA(): Unit = {
    val cd = EvaluateHelper.parseClass("""
class A extends Object {
    A(){super();}
}
    """)

    CT.add("A", cd)
  }
  def addClassB(): Unit = {
    val cd = EvaluateHelper.parseClass("""
class B extends Object {
    B(){super();}
}
    """)

    CT.add("B", cd)
  }
  def addClassPair(): Unit = {
    val cd = EvaluateHelper.parseClass("""
class Pair extends Object {
    Object fst;
    Object snd;
    Pair(Object fst, Object snd) {
        super();
        this.fst = fst;
        this.snd = snd;
    }
    Pair setfst(Object newfst) {
        return new Pair(newfst, this.snd);
    }
}
    """)

    CT.add("Pair", cd)
  }
  def addClassP0(): Unit = {
    val cd = EvaluateHelper.parseClass("""
class P0 extends Object {
    P0(){super();}
}
    """)

    CT.add("P0", cd)
  }
  def addClassP1(): Unit = {
    val cd = EvaluateHelper.parseClass("""
class P1 extends P0 {
    P1(){super();}
}
    """)

    CT.add("P1", cd)
  }
  def addClassP2(): Unit = {
    val cd = EvaluateHelper.parseClass("""
class P2 extends P1 {
    P2(){super();}
}
    """)

    CT.add("P2", cd)
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
  def parseClass(input: String): ClassDef = {
    val parser = FJ.phrase(FJ.ClsDef)
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
