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

  def lastLine(expected: String)(output: String): Unit = {
    """(.*)$""".r findFirstIn output match {
      case Some(lastLine) => lastLine shouldBe (expected)
      case e => throw new Exception("smallStepRed returned <<" + output.toString + ">> and it seams like empty string")
    }
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

  }

  // TODO: Test failures

  // TODO test system output string format
}