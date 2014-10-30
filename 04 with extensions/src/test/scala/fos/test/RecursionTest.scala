package fos.test

import org.scalatest._

class RecursionTest extends FlatSpec with Matchers {
  import fos.{ SimplyTyped, Term }
  import fos.test.helpers.Helper
  import Helper.termParser

  def reduce(t: Term): Stream[Term] = {
    try {
      val u = SimplyTyped.reduce(t)
      info(s"new form: $u")
      t #:: u #:: reduce(u)
    } catch {
      case SimplyTyped.NoRuleApplies(_) => t #:: Stream.empty
    }
  }

  // Doesn't test infinite recursion!!
  def test(input: String, answer: String) {
    it should s"evaluate $input to $answer" in {
      val ast = Helper.parseOrFail(input)
      info(s"the initial ast is: $ast")
      val eval = reduce(ast)
      val value = eval.last.toString
      info(s"the computed value is: $value")
      assert(value === answer)
      info("🍺")
    }
  }

  val tests =
    """fix \x: Nat. 0""" -> "0" ::
      Nil

  behavior of "Our compiler with recursive function"
  tests foreach { case (input, answer) => test(input, answer) }
}
