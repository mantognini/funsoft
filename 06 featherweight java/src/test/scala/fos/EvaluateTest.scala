package fos

import org.scalatest._
import scala.annotation.tailrec

case class EvaluateTestError(str: String) extends Exception(str)

class EvaluateTest extends WordSpec with Matchers {

  def evaluate(input: String): Expr = {
    val ast = parseExpr(input)
    val typ = Type.typeOf(ast)(Type.emptyContext) // Make sure it typechecks first
    val expr = Evaluate(ast)
    expr
  }

  def testSteps(steps: List[String]) {
    // No exception should be thrown, except for the last one.
    // At each steps, the next one should be the output of the current one.

    @tailrec
    def walk(steps: List[String]): Unit = steps match {
      case Nil =>
        fail("unexpected empty steps")

      case step :: Nil =>
        val exception = intercept[NoRuleApplies] {
          val result = evaluate(step)
          fail(s"`$step` was evaluated to `$result` but it was expected to be the last evaluation step")
        }
        info(s"got stuck on $step as expected!")

      case current :: next :: tail =>
        val result = evaluate(current)
        assert(result == parseExpr(next))
        info(s"evaluated $current to $result as expected")
        walk(next :: tail)
    }

    walk(steps)
  }

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

    validTestCases foreach { steps =>
      s"successfully evaluate expression $steps" in {
        testSteps(steps)
        info("🍺")
      }
    }

    stuckCases foreach { lastStep =>
      s"get stuck on expression $lastStep" in {
        testSteps(lastStep :: Nil)
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
}
