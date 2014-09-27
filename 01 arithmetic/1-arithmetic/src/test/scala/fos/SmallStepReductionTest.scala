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

  def testLL = ttools.testReduction(testRed, lastLine)_
  def testST = ttools.testReduction(testRed, stuckTerm)_
  def testME = ttools.testReduction(testRed, matchExactly)_

  "Values" should "be left as it" in {
    ttools.getSomeEvaluationTestingValues()("values") foreach (x => testLL(x._1, x._2))
  }

  "If statement" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("if") foreach (x => testLL(x._1, x._2))
  }

  "Pred" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("pred") foreach (x => testLL(x._1, x._2))
  }

  "IsZero" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("isZero") foreach (x => testLL(x._1, x._2))
  }

  "Congruences" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("smallStepCongruences") foreach (x => testLL(x._1, x._2))
  }

  "More complex compositions" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("complex") foreach (x => testLL(x._1, x._2))
  }

  "unreduceable inputs" should "produce an stuck term message" in {
    testST(Succ(False), "Succ(False)")
    testST(Pred(Succ(Succ(Succ(False)))), "Pred(Succ(Succ(Succ(False))))") // Example from the statement
    testST(IsZero(If(True, False, True)), "IsZero(False)") // Example from forum
    testST(If(False, True, IsZero(True)), "IsZero(True)")
    testST(Pred(False), "Pred(False)")
    testST(If(IsZero(Pred(Succ(Zero))), Succ(True), Succ(False)), "Succ(True)")
    testST(Succ(IsZero(Succ(Zero))), "Succ(False)")
  }

  "string output" should "match specifications" in {
    // No evaluation step (Values)
    testME(False, "False\n")
    testME(True, "True\n")
    testME(Zero, "Zero\n")
    testME(Succ(Zero), "Succ(Zero)\n")
    testME(Succ(Succ(Zero)), "Succ(Succ(Zero))\n")

    // 1 or more evaluation steps
    testME(IsZero(Zero), "IsZero(Zero)\nTrue\n")
    testME(If(IsZero(Succ(Zero)), True, Pred(Zero)),
      """If(IsZero(Succ(Zero)),True,Pred(Zero))
If(False,True,Pred(Zero))
Pred(Zero)
Zero
""")
    testME(If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False),
      """If(IsZero(Pred(Pred(Succ(Succ(Zero))))),If(IsZero(Zero),True,False),False)
If(IsZero(Pred(Succ(Zero))),If(IsZero(Zero),True,False),False)
If(IsZero(Zero),If(IsZero(Zero),True,False),False)
If(True,If(IsZero(Zero),True,False),False)
If(IsZero(Zero),True,False)
If(True,True,False)
True
""")
  }
}