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
  
  "evaluation" should "produce the expected result" in {
    // Values should be left as it
//    testEval(True)(shouldProduce("True"))
//    testEval(True)(shouldProduce("True"))
  }
  
  "unreduceable inputs" should "fail and produce right StuckTern" in {
    // TODO
  }
  
}