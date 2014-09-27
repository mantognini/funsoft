package fos

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.ByteArrayOutputStream

class BigStepRedTest extends FlatSpec with Matchers {

  def testEval(input: Term)(assertFun: String => Unit): Unit = {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Arithmetic.bigStepEvaluation(input)
    }
    assertFun(output.toString)
  }

  def shouldProduce(expected: String)(evalOutput: String): Unit = {
    ("Big step: " + expected) shouldBe evalOutput
  }

  def shouldStuckWith(expected: String)(evalOutput: String): Unit = {
    ("Big step: Stuck term: " + expected) shouldBe evalOutput
  }

  def testSP = ttools.testReduction(testEval, shouldProduce)_
  def testSW = ttools.testReduction(testEval, shouldStuckWith)_

  "Values" should "be left as it" in {
    ttools.getSomeEvaluationTestingValues()("values") foreach (x => testSP(x._1, x._2))
  }

  "If statement" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("if") foreach (x => testSP(x._1, x._2))
  }

  "Pred" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("pred") foreach (x => testSP(x._1, x._2))
  }

  "IsZero" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("isZero") foreach (x => testSP(x._1, x._2))
  }

  "More complex compositions" should "be properly reduced" in {
    ttools.getSomeEvaluationTestingValues()("complex") foreach (x => testSP(x._1, x._2))
  }

  "unreduceable inputs" should "fail and produce right StuckTern" in {
    testSW(Succ(IsZero(Zero)), "Succ(IsZero(Zero))")
    testSW(Pred(Succ(Succ(Succ(False)))), "Succ(False)") // Example from the statement
    testSW(IsZero(If(True, False, True)), "IsZero(If(True, False, True))") // Example from forum
    // TODO more cases?
  }

}