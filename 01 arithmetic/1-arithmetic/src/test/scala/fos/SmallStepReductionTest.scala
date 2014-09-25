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
    testReducedResult(If(True, True, False), "True")
    testReducedResult(If(False, True, False), "False")
    testReducedResult(If(If(True, True, False), True, False), "True")
    testReducedResult(If(If(False, True, False), True, False), "False")
    testReducedResult(If(If(True, True, False), If(False, True, False), False), "False")
  }

  "Pred" should "be properly reduced" in {
    testReducedResult(Pred(Zero), "Zero")
    testReducedResult(Pred(Succ(Zero)), "Zero")
  }

  "IsZero" should "be properly reduced" in {
    testReducedResult(IsZero(Zero), "True")
    testReducedResult(IsZero(Succ(Zero)), "False")
    testReducedResult(IsZero(Succ(Pred(Zero))), "False")
  }

  "Congruences" should "be properly reduced" in {
    testReducedResult(If(IsZero(Zero), True, False), "True")
    testReducedResult(If(IsZero(Pred(Succ(Zero))), True, False), "True")
    testReducedResult(If(IsZero(Succ(Pred(Zero))), True, False), "False")

    testReducedResult(IsZero(Pred(Zero)), "True")
    testReducedResult(IsZero(Pred(Succ(Zero))), "True")
    testReducedResult(IsZero(Pred(Succ(Succ(Zero)))), "False")

    testReducedResult(Pred(Pred(Succ(Succ(Succ(Zero))))), "Succ(Zero)")

    testReducedResult(Succ(Pred(Pred(Succ(Zero)))), "Succ(Zero)")
  }

  "More complex compositions" should "be properly reduced" in {
    testReducedResult(If(IsZero(Pred(Succ(Zero))), Succ(Pred(Zero)), Pred(Zero)), "Succ(Zero)")
    testReducedResult(IsZero(If(If(IsZero(Succ(Zero)), True, False), True, Succ(Zero))), "False")
  }

  // TODO: Test failures

  // TODO test system output string format
}