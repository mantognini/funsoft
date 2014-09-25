package fos

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.ByteArrayOutputStream

class SmallStepReductionTest extends FlatSpec with Matchers {

  def testReducedResult(input: Term, finalResult: String) {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Arithmetic.smallStepRed(input)
    }
    """(.*)$""".r findFirstIn output.toString match {
      case Some(lastLine) => lastLine shouldBe (finalResult)
      case e => throw new Exception("smallStepRed returned <<" + output.toString + ">> and it seams like empty string")
    }
  }

  "Values" should "be left as it" in {
    testReducedResult(False, "False")
    testReducedResult(True, "True")
    testReducedResult(Zero, "Zero")
    testReducedResult(Succ(Zero), "Succ(Zero)")
    testReducedResult(Succ(Succ(Zero)), "Succ(Succ(Zero))")
  }

  "If statement" should "be properly reduced" in {
    testReducedResult(If(True, True, True), "True")
  }

}