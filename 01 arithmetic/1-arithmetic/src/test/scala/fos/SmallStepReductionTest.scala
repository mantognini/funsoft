package fos

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.ByteArrayOutputStream

class SmallStepReductionTest extends FlatSpec with Matchers {

  def testRed(input: Term)(assertFun: String => Unit): Unit = {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Arithmetic.smallStepRed(input)
    }
    assertFun(output.toString)
  }

  def lastLine(expected: String)(reductionOutput: String): Unit = {
    """(.*)$""".r findFirstIn reductionOutput match {
      case Some(lastLine) => lastLine shouldBe (expected)
      case e => throw new Exception("smallStepRed returned <<" + reductionOutput.toString + ">> and it seams like empty string")
    }
  }

  def stuckTerm(expected: String)(reductionOutput: String): Unit = {
    """Stuck term: (.*)""".r findFirstMatchIn reductionOutput match {
      case Some(stuckExpr) => stuckExpr.group(1) shouldBe (expected)
      case e => reductionOutput should contain("a well-formed stuck term output")
    }
  }
  
  def matchExactly(expected: String)(reductionOutput: String): Unit = {
    reductionOutput shouldBe expected
  }

  "Values" should "be left as it" in {
    testRed(False)(lastLine("False"))
    testRed(True)(lastLine("True"))
    testRed(Zero)(lastLine("Zero"))
    testRed(Succ(Zero))(lastLine("Succ(Zero)"))
    testRed(Succ(Succ(Zero)))(lastLine("Succ(Succ(Zero))"))
  }

  "If statement" should "be properly reduced" in {
    testRed(If(True, True, False))(lastLine("True"))
    testRed(If(False, True, False))(lastLine("False"))
    testRed(If(If(True, True, False), True, False))(lastLine("True"))
    testRed(If(If(False, True, False), True, False))(lastLine("False"))
    testRed(If(If(True, True, False), If(False, True, False), False))(lastLine("False"))
  }

  "Pred" should "be properly reduced" in {
    testRed(Pred(Zero))(lastLine("Zero"))
    testRed(Pred(Succ(Zero)))(lastLine("Zero"))
  }

  "IsZero" should "be properly reduced" in {
    testRed(IsZero(Zero))(lastLine("True"))
    testRed(IsZero(Succ(Zero)))(lastLine("False"))
    testRed(IsZero(Succ(Pred(Zero))))(lastLine("False"))
  }

  "Congruences" should "be properly reduced" in {
    testRed(If(IsZero(Zero), True, False))(lastLine("True"))
    testRed(If(IsZero(Pred(Succ(Zero))), True, False))(lastLine("True"))
    testRed(If(IsZero(Succ(Pred(Zero))), True, False))(lastLine("False"))

    testRed(IsZero(Pred(Zero)))(lastLine("True"))
    testRed(IsZero(Pred(Succ(Zero))))(lastLine("True"))
    testRed(IsZero(Pred(Succ(Succ(Zero)))))(lastLine("False"))

    testRed(Pred(Pred(Succ(Succ(Succ(Zero))))))(lastLine("Succ(Zero)"))

    testRed(Succ(Pred(Pred(Succ(Zero)))))(lastLine("Succ(Zero)"))
  }

  "More complex compositions" should "be properly reduced" in {
    testRed(If(IsZero(Pred(Succ(Zero))), Succ(Pred(Zero)), Pred(Zero)))(lastLine("Succ(Zero)"))
    testRed(IsZero(If(If(IsZero(Succ(Zero)), True, False), True, Succ(Zero))))(lastLine("False"))
  }

  "unreduceable inputs" should "produce an stuck term message" in {
    testRed(Succ(False))(stuckTerm("Succ(False)"))
    testRed(Pred(Succ(Succ(Succ(False)))))(stuckTerm("Pred(Succ(Succ(Succ(False))))"))
    testRed(IsZero(If(True, False, True)))(stuckTerm("IsZero(False)"))
    testRed(If(False, True, IsZero(True)))(stuckTerm("IsZero(True)"))
    testRed(Pred(False))(stuckTerm("Pred(False)"))
    testRed(If(IsZero(Pred(Succ(Zero))), Succ(True), Succ(False)))(stuckTerm("Succ(True)"))
    testRed(Succ(IsZero(Succ(Zero))))(stuckTerm("Succ(False)"))
  }

  "string output" should "match specifications" in {
    // No evaluation step (Values)
    testRed(False)(matchExactly("False\n"))
    testRed(True)(matchExactly("True\n"))
    testRed(Zero)(matchExactly("Zero\n"))
    testRed(Succ(Zero))(matchExactly("Succ(Zero)\n"))
    testRed(Succ(Succ(Zero)))(matchExactly("Succ(Succ(Zero))\n"))
    
    // 1 or more evaluation steps
    testRed(IsZero(Zero))(matchExactly("IsZero(Zero)\nTrue\n"))
    testRed(If(IsZero(Succ(Zero)), True, Pred(Zero)))(matchExactly(
"""If(IsZero(Succ(Zero)),True,Pred(Zero))
If(False,True,Pred(Zero))
Pred(Zero)
Zero
"""
    ))
    testRed(If(IsZero(Pred(Pred(Succ(Succ(Zero))))),If(IsZero(Zero),True,False),False))(matchExactly(
"""If(IsZero(Pred(Pred(Succ(Succ(Zero))))),If(IsZero(Zero),True,False),False)
If(IsZero(Pred(Succ(Zero))),If(IsZero(Zero),True,False),False)
If(IsZero(Zero),If(IsZero(Zero),True,False),False)
If(True,If(IsZero(Zero),True,False),False)
If(IsZero(Zero),True,False)
If(True,True,False)
True
"""
    ))
  }
}