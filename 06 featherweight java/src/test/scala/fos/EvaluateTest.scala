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

  "The Evaluator" should {
    s"successfully initialise the context with a few classes" in {
      CT.clear()
      addClassA()
      addClassB()
      addClassPair()
      addClassP0()
      addClassP1()
      addClassP2()
    }

    validTestCases foreach { t =>
      s"successfully evaluate expression $t" in {
        test(t) should be(None)
        info("🍺")
      }
    }

    stuckCases foreach { t =>
      s"get stuck on expression $t" in {
        test(t :: Nil) should be(None)
        info("🍺")
      }
    }
  }

  def addClassA(): Unit = {
    addClass("""
class A extends Object {
    A(){super();}
}
""")
  }

  def addClassB(): Unit = {
    addClass("""
class B extends Object {
    B(){super();}
}
""")
  }

  def addClassPair(): Unit = {
    addClass("""
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
  }

  def addClassP0(): Unit = {
    addClass("""
class P0 extends Object {
    P0(){super();}
}
""")
  }

  def addClassP1(): Unit = {
    addClass("""
class P1 extends P0 {
    P1(){super();}
}
""")
  }

  def addClassP2(): Unit = {
    addClass("""
class P2 extends P1 {
    P2(){super();}
}
""")
  }

  def addClass(code: String) {
    val ast = parseClass(code)
    Type.typeOf(ast)(Type.emptyContext) // make sure it typechecks
    // When typecheck is ok, the class is added to CT
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
    val ast = parseExpr(input)
    val expr = Evaluate(ast)
    expr
  }

  def test(steps: List[String]): Option[String] = {
    def testAStep(step: String, expected: Option[String]): Option[String] = {
      info(s"info: testing that `$step` evaluates to `$expected`")
      expected match {
        case Some(nextStep) => {
          try {
            val effectiveNextStep = evaluate(step)
            if (effectiveNextStep == parseExpr(nextStep)) {
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
