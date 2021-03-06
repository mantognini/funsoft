package fos

import org.scalatest._
import java.io.ByteArrayOutputStream

class BigStepRedTest extends WordSpec with Matchers {

  def testEval(input: Term)(assertFun: String => Unit): Unit = {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Arithmetic.eval(input)
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
  val failtests =
    Map(
      Succ(IsZero(Zero)) -> "Succ(IsZero(Zero))",
      Succ(IsZero(Pred(False))) -> "Pred(False)",
      Pred(Succ(Succ(Succ(False)))) -> "Succ(False)", // Example from the statement
      Pred(Succ(Succ(False))) -> "Succ(False)",
      Pred(Succ(False)) -> "Succ(False)",
      IsZero(Pred(False)) -> "Pred(False)",
      IsZero(If(Succ(Zero), True, False)) -> "If(Succ(Zero),True,False)",
      IsZero(If(True, False, True)) -> "IsZero(If(True,False,True))", // Example from forum
      If(False, True, Succ(False)) -> "Succ(False)",
      If(True, Succ(True), False) -> "Succ(True)",
      If(True, IsZero(IsZero(Zero)), Succ(True)) -> "IsZero(IsZero(Zero))",
      If(Pred(Zero), True, True) -> "If(Pred(Zero),True,True)")

  "BigStep evaluator" when {
    for ((category, subtests) <- tests) {
      "term is in category " + category + " and is well defined" when {
        for (test <- subtests) {
          "term is " + test._1 should {
            "produce " + test._2 in {
              testSP(test._1, test._2)
            }
          }
        }
      }
    }

    "input is invalid" should {
      for (failtest <- failtests) {
        "print «Stuck term: " + failtest._2 + "» with " + failtest._1 in {
          testSW(failtest._1, failtest._2)
        }
      }
    }
  }

}
