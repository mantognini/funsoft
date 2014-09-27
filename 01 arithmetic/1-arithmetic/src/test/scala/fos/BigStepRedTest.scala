package fos

import org.scalatest._
import java.io.ByteArrayOutputStream

class BigStepRedTest extends WordSpec with Matchers {

  def testEval(input: Term)(assertFun: String => Unit): Unit = {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Arithmetic.bigStepEvaluation(input)
    }
    assertFun(output.toString)
  }

  def shouldProduce(expected: String)(evalOutput: String): Unit = {
    evalOutput shouldBe ("Big step: " + expected)
  }

  def shouldStuckWith(expected: String)(evalOutput: String): Unit = {
    evalOutput shouldBe ("Big step: Stuck term: " + expected)
  }

  def testSP = ttools.testReduction(testEval, shouldProduce)_
  def testSW = ttools.testReduction(testEval, shouldStuckWith)_

  val tests = ttools.getSomeEvaluationTestingValues()
  "Terms" when {
    for ((category, subtests) <- tests) {
      category should {
        for (x <- subtests) {
          "properly reduce " + x._1 in {
            testSP(x._1, x._2)
          }
        }
      }
    }
  }

  // TODO reactivate those test
  //  "unreduceable inputs" should "fail and produce right StuckTern" in {
  //    testSW(Succ(IsZero(Zero)), "Succ(IsZero(Zero))")
  //    testSW(Pred(Succ(Succ(Succ(False)))), "Succ(False)") // Example from the statement
  //    testSW(IsZero(If(True, False, True)), "IsZero(If(True, False, True))") // Example from forum
  //    // TODO more cases?
  //  }

}