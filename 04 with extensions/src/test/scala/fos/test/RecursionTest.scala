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
      val ast = Helper.parseAndCheckOrFail(input)
      val expectedAst = Helper.parseAndCheckOrFail(answer)
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

  val timesDef =
    plusDef + """in
    letrec times : Nat -> Nat -> Nat
          = \m: Nat. \n: Nat.
            if iszero m then 0 else plus n (times (pred m) n)
    """

  val factorialDef =
    timesDef + """in
    letrec factorial : Nat -> Nat
        = \m: Nat.
          if iszero m then 1 else times m (factorial (pred m))
    """

  val tests =
    """fix \x: Nat. 0""" -> "0" ::
      s"$plusDef in plus 0 0" -> "0" ::
      s"$plusDef in plus 5 5" -> "10" ::
      s"$plusDef in plus 9 1" -> "10" ::
      s"$timesDef in times 1 10" -> "10" ::
      s"$timesDef in times 2 10" -> "20" ::
      s"$timesDef in times 5 10" -> "50" ::
      s"$timesDef in times (times 2 3) 7" -> "42" ::
      s"$timesDef in times 9 10" -> "90" ::
      s"$factorialDef in factorial 4" -> "24" ::
      s"$factorialDef in factorial 5" -> "120" ::
      Nil

  behavior of "Our compiler with recursive function"
  tests foreach { case (input, answer) => test(input, answer) }

  // test(s"$factorialDef in factorial 10", "3628800")
  // 10! produces a stack overflow at fos.SimplyTyped$.convertNumeric(SimplyTyped.scala:16))
  // or a java.lang.OutOfMemoryError when convertNumeric is properly fixed
  // Not sure if it's because of eclipse or scala test, though.
}
