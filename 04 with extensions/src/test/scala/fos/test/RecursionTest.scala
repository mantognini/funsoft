package fos.test

import org.scalatest._

class RecursionTest extends FlatSpec with Matchers {
  import fos.{ SimplyTyped, Term }
  import fos.test.helpers.Helper
  import Helper.termParser

  def reduce(t: Term): Stream[Term] = {
    try {
      val u = SimplyTyped.reduce(t)
      t #:: u #:: reduce(u)
    } catch {
      case SimplyTyped.NoRuleApplies(_) => t #:: Stream.empty
    }
  }

  // Doesn't test infinite recursion!!
  def test(input: String, answer: String) {
    it should s"evaluate $input to $answer" in {
      val ast = Helper.parseOrFail(input)
      val eval = reduce(ast)
      val value = eval.last
      assert(value === answer)
    }
  }

  val tests =
    """ """ -> """ """ ::
      Nil

  behavior of "Our compiler with recursive function"
  tests foreach { case (input, answer) => test(input, answer) }
}
