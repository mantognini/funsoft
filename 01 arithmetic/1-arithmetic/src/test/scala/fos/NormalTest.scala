package fos

import org.scalatest._

class NormalTest extends FlatSpec with Matchers {

  "Zero" should "be normal" in {
    assert(Arithmetic.isNormal(Zero))
  }

  "sequence of succ" should "be normal" in {
    assert(Arithmetic.isNormal(Succ(Zero)))
    assert(Arithmetic.isNormal(Succ(Succ(Zero))))
  }

  "term with pred" should "NOT be normal" in {
    assert(!Arithmetic.isNormal(Pred(Zero)))
    assert(!Arithmetic.isNormal(Pred(Succ(Zero))))
    assert(!Arithmetic.isNormal(Pred(Pred(Zero))))
    assert(!Arithmetic.isNormal(Succ(Pred(Zero))))
    assert(!Arithmetic.isNormal(Succ(Succ(Pred(Zero)))))
  }

  "true" should "be normal" in {
    assert(Arithmetic.isNormal(True))
  }

  "false" should "be normal" in {
    assert(Arithmetic.isNormal(False))
  }

  "if" should "NOT be normal" in {
    assert(!Arithmetic.isNormal(If(True, True, True)))
    assert(!Arithmetic.isNormal(If(False, True, True)))
    assert(!Arithmetic.isNormal(If(True, Succ(Zero), True)))
    assert(!Arithmetic.isNormal(If(True, True, If(True, True, True))))
    assert(!Arithmetic.isNormal(If(If(IsZero(True), False, Succ(Succ(True))), IsZero(Zero), Succ(False))))
  }

  "iszero" should "NOT be normal" in {
    assert(!Arithmetic.isNormal(IsZero(True)))
    assert(!Arithmetic.isNormal(IsZero(False)))
    assert(!Arithmetic.isNormal(IsZero(Zero)))
    assert(!Arithmetic.isNormal(IsZero(Succ(Zero))))
  }

}