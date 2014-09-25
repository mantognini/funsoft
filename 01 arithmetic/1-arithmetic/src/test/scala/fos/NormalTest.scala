package fos

import org.scalatest._

class NormalTest extends FlatSpec with Matchers {

  "Zero" should "be normal" in {
    assert(Arithmetic.isNormal(Zero))
  }

  "1" should "be normal" in {
    assert(Arithmetic.isNormal(Succ(Zero)))
  }

  "2" should "be normal" in {
    assert(Arithmetic.isNormal(Succ(Succ(Zero))))
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
  }

  "iszero" should "NOT be normal" in {
    assert(!Arithmetic.isNormal(IsZero(True)))
    assert(!Arithmetic.isNormal(IsZero(False)))
    assert(!Arithmetic.isNormal(IsZero(Zero)))
    assert(!Arithmetic.isNormal(IsZero(Succ(Zero))))
  }

}