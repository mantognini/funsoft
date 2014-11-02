package fos.test

object SumTypesTest {

  import fos.SimplyTyped
  import fos.{ Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Pair, First, Second, Inl, Inr, Case }
  import fos.{ Type, Bool, Nat, Function, Product, Sum }
  import fos.test.helpers.Helper._

  val simpleSteps: List[Tuple2[List[String], Type]] = List(
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
      """succ succ zero""" :: Nil, Nat()))

  val advancedSteps: List[Tuple2[List[String], Type]] = List(
    ("""inl {zero, false} as (Nat*Bool)+(Nat->Nat)""" :: Nil, Sum(Product(Nat(), Bool()), Function(Nat(), Nat()))),
    ("""inl {zero, \x:Nat.iszero x} as (Nat*(Nat->Bool))+Nat""" :: Nil, Sum(Product(Nat(), Bool()), Nat())),
    ("""iszero case if true then inl zero as Nat+Bool else inr iszero succ zero as Nat+Bool of inl x => succ x | inr x => if x then zero else succ zero""" ::
      """iszero case inl zero as Nat+Bool of inl x => succ x | inr x => if x then zero else succ zero""" ::
      """iszero succ zero""" ::
      """false""" :: Nil, Bool()),
    ("""inl inl zero as Nat+Bool as (Nat+Bool)+Nat""" :: Nil, Sum(Sum(Nat(), Bool()), Nat())),
    ("""inr inl true as Bool+Nat as (Nat->Bool)+Bool+Nat""" :: Nil, Sum(Function(Nat(), Bool()), Sum(Bool(), Nat()))),
    ("""case inr inl true as Bool+Nat as (Nat->Bool)+Bool+Nat of inl x => zero | inr y => case y of inl y => if y then zero else succ zero | inr y => succ y""" ::
      """case inl true as Bool+Nat of inl y => if y then zero else succ zero | inr y => succ y""" ::
      """if true then zero else succ zero""" ::
      """zero""" :: Nil, Nat()))

  val stepsAndFinalType = simpleSteps ::: advancedSteps

  val stepsCases: List[List[String]] = stepsAndFinalType map { _._1 }
  val typeCheckPositives: List[Tuple2[String, Type]] = stepsAndFinalType map { p => (p._1.head, p._2) }

  val dontTypeCheck: List[Term] = List(
    // Inl | inr has to be a Sum(_,_)
    Inr(True(), Bool()),
    Inr(Zero(), Nat()),
    Inl(Abs(x, Nat(), Zero()), Function(Nat(), Nat())),

    // << Inl | Inr as T1+T2 >> terms have to match their sum-type T1+T2
    Inr(True(), Sum(Bool(), Nat())),
    Inl(Succ(Zero()), Sum(Bool(), Nat())),

    // case term has to be a Sum(_,_)
    Case(False(), x, Bool(), x, Bool()),
    Case(IsZero(Zero()), x, Bool(), x, Bool()),
    Case(If(True(), True(), Zero()), x, Bool(), y, Bool()),

    // t1 amd t2 have to be of same type
    Case(Inr(True(), Sum(Nat(), Bool())), x, True(), x, Zero()),
    Case(Inl(Zero(), Sum(Nat(), Nat())), y, IsZero(Zero()), z, Zero()))

  val dontTypeCheckStrings: List[String] = dontTypeCheck map { _.toString }

  val typeToStringCases: List[Tuple2[Term, String]] = List(
    // + has same precedence as *
    (Sum(Bool(), Product(Nat(), Sum(Bool(), Nat()))), "Bool+Nat*Bool+Nat"))

  val termsToStringCases: List[Tuple2[Term, String]] = List(
    // Cases (with inl / inr)
    (Case(Inr(True(), Sum(Nat(), Bool())), x, Succ(x), y, Zero()),
      "case inr true as Nat+Bool of inl x=>succ x | inr y=>0"),

    // Bruce lee combo
    (Case(Inr(Inl(True(), Sum(Bool(), Nat())), Sum(Function(Nat(), Bool()), Sum(Bool(), Nat()))), x, Zero(), y, Case(y, y, If(y, Zero(), Succ(Zero())), y, Succ(y))),
      "case inr inl true as Bool+Nat as (Nat->Bool)+Bool+Nat of inl x=>0 | inr y=>case y of inl y=>if y then 0 else succ 0 | inr y=>succ y"))

  val toStringCases = typeToStringCases ::: termsToStringCases

  val parserTypeCases: List[Tuple2[String, Term]] = typeToStringCases map { _.swap }
  val parserTermCases: List[Tuple2[String, Term]] = termsToStringCases map { _.swap }

}