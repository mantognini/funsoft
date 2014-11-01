package fos.test

import org.scalatest._

class SumTypesTest extends WordSpec with Matchers {

  import fos.SimplyTyped
  import fos.{ Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Pair, First, Second }
  import fos.{ Type, Bool, Nat, Function, Product, Sum }
  import fos.test.helpers.Helper._

  def steps: List[Tuple2[List[String], Type]] = List(
    ("""inl false as Bool+Nat""" :: Nil, Sum(Bool(), Nat())),
    ("""inr true as Nat+Bool""" :: Nil, Sum(Nat(), Bool())),
    ("""inr pred succ succ zero as Nat+Nat""" :: """inr succ zero as Nat+Nat""" :: Nil, Sum(Nat(), Nat())),
    ("""inl fst {true, zero} as Bool+Nat""" :: """inl true as Bool+Nat""" :: Nil, Sum(Bool(), Nat())),
    ("""case inl false of inl x => x | inr y => iszero y""" :: """false""" :: Nil, Bool()),
    ("""case inr zero of inl x => zero | inr y => succ y""" :: """succ zero""" :: Nil, Nat()),
    ("""case if true then inl succ zero else inr false of inl x => iszero x | inr y => if y then false else true""" ::
      """case inl succ zero of inl x => iszero x | inr y => if y then false else true""" ::
      """iszero succ zero""" :: """false""" :: Nil, Bool()),
    ("""case inl if false then zero else succ zero of inl x => succ x | inr y => zero""" ::
      """case inl succ zero of inl x => succ x | inr y => zero""" ::
      """succ succ zero""" :: Nil, Nat()),
    (Nil, ???))

  def types: List[Type] = List()
}
