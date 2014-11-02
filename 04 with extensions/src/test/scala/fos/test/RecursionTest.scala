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
      val expectedAst = Helper.parseOrFail(answer)
      info(s"the initial ast is: $ast")
      val eval = reduce(ast)
      val finalAst = eval.last
      info(s"the computed value is: $finalAst")
      assert(finalAst === expectedAst)
      info("🍺")
    }
  }

  // Recursion with fix, see TAPL §11.11.1 and §11.11.2 (pp144-145 + pp510-511)

  val plusDef =
    """
    letrec plus: Nat -> Nat -> Nat
          = \m: Nat. \n: Nat.
            if iszero m then n else succ (plus (pred m) n)
    """

  val timesDef = s"$plusDef in " +
    """
    letrec times : Nat -> Nat -> Nat
          = \m: Nat. \n: Nat.
            if iszero m then 0 else plus n (times (pred m) n)
    """

  val tests =
    """fix \x: Nat. 0""" -> "0" ::
      s"$plusDef in plus 0 0" -> "0" ::
      s"$plusDef in plus 5 5" -> "10" ::
      s"$plusDef in plus 9 1" -> "10" ::
      s"$timesDef in times 9 10" -> "90" ::
      Nil

  behavior of "Our compiler with recursive function"
  tests foreach { case (input, answer) => test(input, answer) }
}
