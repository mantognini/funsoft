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
      Nil

  def testNegative(input: String) {
    s"fire an exception for untyped input: $input" in {
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
