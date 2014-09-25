package fos

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.ByteArrayOutputStream

class SmallStepReductionTest extends FlatSpec with Matchers {

  def testToReduce(input: Term, expectedOutput: String) {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Arithmetic.smallStepRed(input)
    }
    output.toString.trim shouldBe (expectedOutput)
  }

  "Values" should "be left as it" in {
    testToReduce(False, "False")
    testToReduce(True, "True")
    testToReduce(Zero, "Zero")
    testToReduce(Succ(Zero), "Succ(Zero)")
  }

}