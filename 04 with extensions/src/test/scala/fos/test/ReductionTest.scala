package fos.test

import org.scalatest._
import fos.test.helpers.Helper
import fos.SimplyTyped
import fos.Term
import scala.util.Random

class ReductionTest extends FlatSpec with Matchers {

  import fos.test.SumTypesTest

  val ValidAndTerminating: List[List[String]] = SumTypesTest.stepsCases ::: List(
    """\x:Bool.x""" :: Nil,
    """\x:Nat.\y:Bool.y""" :: Nil,
    """\x:Bool->Nat.\y:Bool.x y""" :: Nil,

    // Variables are not considered as values
    """\y:Bool.(\x:Bool.x) y""" :: Nil,

    """(\z:Nat.(\x:Bool->Bool.x) \y:Bool.y) 0""" :: """(\x: Bool -> Bool. x) \y:Bool.y""" :: """\y: Bool. y""" :: Nil,
    """(\x:Nat->Bool. (\y:Nat.(x y))) (\x:Nat.(iszero x)) 0""" :: """(\y:Nat.(\x:Nat.iszero x) y)  0""" :: """(\x:Nat.iszero x) 0""" :: """iszero 0""" :: """true""" :: Nil,
    Nil)

  behavior of "reduction(call-by-value)"

  def parse(s: String): Term = Helper.parseOrFail(s)(Helper.termParser)
  def reduce(t: Term): Term = SimplyTyped.reduce(t)

  def testSteps(l: List[String], t: Term, stepNo: Int): Unit = l match {
    case Nil => Assertions.fail(s"reduced to $t but no more steps expected")
    case x :: xs => {
      val r = (new Random).nextInt
      it should s"produce $x at step $stepNo ($r)" in {
        assert(t === parse(x))
      }
      try {
        testSteps(xs, reduce(t), stepNo + 1)
      } catch {
        case SimplyTyped.NoRuleApplies(t) => xs shouldBe empty
      }
    }
  }

  ValidAndTerminating foreach (stepList => stepList match {
    case Nil => {}
    case l @ x :: xs => testSteps(l, parse(x), 1)
  })

}
