package fos.test

import org.scalatest._

class SumTypesTest extends WordSpec with Matchers {

  import fos.SimplyTyped
  import fos.{ Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Pair, First, Second }
  import fos.{ Type, Bool, Nat, Function, Product, Sum }
  import fos.test.helpers.Helper._

  def simpleSteps: List[Tuple2[List[String], Type]] = List(
    ("""inl false as Bool+Nat""" :: Nil, Sum(Bool(), Nat())),
    ("""inr true as Nat+Bool""" :: Nil, Sum(Nat(), Bool())),
    ("""inr pred succ succ zero as Nat+Nat""" :: """inr succ zero as Nat+Nat""" :: Nil, Sum(Nat(), Nat())),
    ("""inl fst {true, zero} as Bool+Nat""" :: """inl true as Bool+Nat""" :: Nil, Sum(Bool(), Nat())),
    ("""case inl false as Bool+Nat of inl x => x | inr y => iszero y""" :: """false""" :: Nil, Bool()),
    ("""case inr zero as (Bool*Bool->(Nat->Bool+Nat->Nat)*Bool)+Nat of inl x => zero | inr y => succ y""" ::
      """succ zero""" :: Nil, Nat()),
    ("""case if true then inl succ zero as Nat+Bool else inr false as Nat+Bool of inl x => iszero x | inr y => if y then false else true""" ::
      """case inl succ zero as Nat+Bool of inl x => iszero x | inr y => if y then false else true""" ::
      """iszero succ zero""" :: """false""" :: Nil, Bool()),
    ("""case inl if false then zero else succ zero as Nat+Bool*Nat+Bool*((Nat->Nat)->Bool) of inl x => succ x | inr y => zero""" ::
      """case inl succ zero as Nat+Bool*Nat+Bool*((Nat->Nat)->Bool) of inl x => succ x | inr y => zero""" ::
      """succ succ zero""" :: Nil, Nat()),
    (Nil, ???))

  def advancedSteps: List[Tuple2[List[String], Type]] = List(
    ("""inl {zero, false} as (Nat*Bool)+(Nat->Nat)""" :: Nil, Sum(Product(Nat(), Bool()), Function(Nat(), Nat()))),
    ("""inl {zero, \x:Nat.iszero x} as (Nat*(Nat->Bool))+Nat""" :: Nil, Sum(Product(Nat(), Bool()), Nat())),
    ("""iszero case if true then inl zero as Nat+Bool else inr iszero succ zero as Nat+Bool of inl x => succ x | inr x => if x then zero else succ zero""" ::
      """iszero case inl zero as Nat+Bool of inl x => succ x | inr x => if x then zero else succ zero""" ::
      """iszero succ zero""" ::
      """false""" :: Nil, Bool()),
    (Nil, ???))

  def steps = simpleSteps ::: advancedSteps
}
