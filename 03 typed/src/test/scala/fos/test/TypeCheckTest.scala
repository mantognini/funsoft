package fos.test

import org.scalatest._

class TypeCheckTest extends WordSpec with Matchers {

  import fos.test.helpers.Helper._
  import fos.SimplyTyped
  import fos.SimplyTyped.{ TypeError }
  import fos.{ Type, Bool, Nat, Product, Function }

  // List of test that should fail
  val negatives =
    "x" ::
      "x y" ::
      "pred false" ::
      "succ false" ::
      "iszero false" ::
      "if 0 then 0 else 0" ::
      "if true then true else 0" ::
      """\x: Bool. y""" ::
      """(\x: Bool. x) 0""" ::
      "fst 0" ::
      "snd 0" ::
      Nil

  // List of test that should pass
  val positives =
    "true" -> Bool ::
      "false" -> Bool ::
      "0" -> Nat ::
      "pred 0" -> Nat ::
      "1" -> Nat ::
      "pred 1" -> Nat ::
      "iszero 0" -> Bool ::
      "iszero 1" -> Bool ::
      "if true then 0 else 1" -> Nat ::
      """\x: Bool. true""" -> Function(Bool, Bool) ::
      """\x: Nat. true""" -> Function(Nat, Bool) ::
      """\x: Bool. x""" -> Function(Bool, Bool) :: // This tests x:T ∈ Γ
      """(\x: Bool. x) true""" -> Bool ::
      """(\x: Bool -> Bool. x true) (\x: Bool. x)""" -> Bool ::
      "{ true, true }" -> Product(Bool, Bool) ::
      "{ 0, true }" -> Product(Nat, Bool) ::
      "fst { 0, true }" -> Nat ::
      "snd { 0, true }" -> Bool ::
      """{ \x: Nat. iszero x, \x: Bool. if x then 0 else 1 }""" -> Product(Function(Nat, Bool), Function(Bool, Nat)) ::
      """fst fst { { 0, true }, { \x: Nat. iszero x, \x: Bool. if x then 0 else 1 } }""" -> Nat ::
      """(snd snd { { 0, true }, { \x: Nat. iszero x, \x: Bool. if x then 0 else 1 } }) true""" -> Nat ::
      Nil

  def testNegative(input: String) {
    s"fire an exception for input that doesn't type check: $input" in {
      val tree = parseOrFail(input)
      a[TypeError] should be thrownBy {
        SimplyTyped.typeof(tree)
      }
    }
  }

  def testPositive(test: (String, Type)) {
    val (input, expectedType) = test
    s"deduce the type of $input to be $expectedType" in {
      val tree = parseOrFail(input)
      tryOrFail[TypeError] {
        val typ = SimplyTyped.typeof(tree)
        assert(typ === expectedType)
      }
    }
  }

  "The typechecker" should {
    negatives foreach testNegative
    positives foreach testPositive
  }

}
