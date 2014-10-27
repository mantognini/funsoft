package fos.test

import org.scalatest._
import fos.test.helpers.Helper
import fos.SimplyTyped
import fos.Term

class reductionTest extends FlatSpec with Matchers {

  val ValidAndTerminating: List[List[String]] = List(
    """\x:Bool.x""" :: Nil,
    """\x:Nat.\y:Bool.y""" :: Nil,
    """\x:Bool->Nat.\y:Bool.x y""" :: Nil,

    // Variables are not considered as values
    """\y:Bool.(\x:Bool.x) y""" :: Nil,

    // Controversy: See http://moodle.epfl.ch/mod/forum/discuss.php?d=192740#p438873
    """(\z:Nat.(\x:Bool->Bool.x) \y:Bool.y) 0""" :: """(\z:Nat.\y:Bool.y) 0""" :: """\y:Bool.y""" :: Nil,

    Nil)

  behavior of "reduction(call-by-value)"

  def parse(s: String): Term = Helper.parseOrFail(s)(Helper.termParser)
  def reduce(t: Term): Term = SimplyTyped.reduce(t)

  def testSteps(l: List[String], t: Term, stepNo: Int): Unit = l match {
    case Nil => Assertions.fail(s"reduced to $t but no more steps expected")
    case x :: xs => {
      it should s"produce $x at step $stepNo" in {
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
