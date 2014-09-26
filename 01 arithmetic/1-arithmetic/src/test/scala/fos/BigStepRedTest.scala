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
  
  "evaluation" should "produce the expected result" in {
    // TODO
  }
  
  "unreduceable inputs" should "fail and produce right StuckTern" in {
    // TODO
  }
  
}