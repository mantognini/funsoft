package fos.test

object SumTypesTest {

  import fos.SimplyTyped
  import fos.{ Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Pair, First, Second, Inl, Inr, Case }
  import fos.{ Type, Bool, Nat, Function, Product, Sum }
  import fos.test.helpers.Helper._

  val simpleSteps: List[Tuple2[List[String], Type]] = List(
    ("""inl false as Bool+Nat""" :: Nil, Sum(Bool(), Nat())),
    ("""inr true as Nat+Bool""" :: Nil, Sum(Nat(), Bool())),
    ("""inr pred succ succ 0 as Nat+Nat""" :: """inr succ 0 as Nat+Nat""" :: Nil, Sum(Nat(), Nat())),
    ("""inl fst {true, 0} as Bool+Nat""" :: """inl true as Bool+Nat""" :: Nil, Sum(Bool(), Nat())),
    ("""case inl false as Bool+Nat of inl x => x | inr y => iszero y""" :: """false""" :: Nil, Bool()),
    ("""case inr 0 as (Bool*Bool->(Nat->Bool+Nat->Nat)*Bool)+Nat of inl x => 0 | inr y => succ y""" ::
      """succ 0""" :: Nil, Nat()),
    ("""case if true then inl succ 0 as Nat+Bool else inr false as Nat+Bool of inl x => iszero x | inr y => if y then false else true""" ::
      """case inl succ 0 as Nat+Bool of inl x => iszero x | inr y => if y then false else true""" ::
      """iszero succ 0""" :: """false""" :: Nil, Bool()),
    ("""case inl if false then 0 else succ 0 as Nat+Bool*Nat+Bool*((Nat->Nat)->Bool) of inl x => succ x | inr y => 0""" ::
      """case inl succ 0 as Nat+Bool*Nat+Bool*((Nat->Nat)->Bool) of inl x => succ x | inr y => 0""" ::
      """succ succ 0""" :: Nil, Nat()))

  val advancedSteps: List[Tuple2[List[String], Type]] = List(
    ("""inl {0, false} as (Nat*Bool)+(Nat->Nat)""" :: Nil, Sum(Product(Nat(), Bool()), Function(Nat(), Nat()))),
    ("""inl {0, \x:Nat.iszero x} as (Nat*(Nat->Bool))+Nat""" :: Nil, Sum(Product(Nat(), Function(Nat(), Bool())), Nat())),
    ("""iszero case if true then inl 0 as Nat+Bool else inr iszero succ 0 as Nat+Bool of inl x => succ x | inr x => if x then 0 else succ 0""" ::
      """iszero case inl 0 as Nat+Bool of inl x => succ x | inr x => if x then 0 else succ 0""" ::
      """iszero succ 0""" ::
      """false""" :: Nil, Bool()),
    ("""inl inl 0 as Nat+Bool as (Nat+Bool)+Nat""" :: Nil, Sum(Sum(Nat(), Bool()), Nat())),
    ("""inr inl true as Bool+Nat as (Nat->Bool)+Bool+Nat""" :: Nil, Sum(Function(Nat(), Bool()), Sum(Bool(), Nat()))),
    ("""case inr inl true as Bool+Nat as (Nat->Bool)+Bool+Nat of inl x => 0 | inr y => case y of inl y => if y then 0 else succ 0 | inr y => succ y""" ::
      """case inl true as Bool+Nat of inl y => if y then 0 else succ 0 | inr y => succ y""" ::
      """if true then 0 else succ 0""" ::
      """0""" :: Nil, Nat()))

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
    Case(False(), x, True(), x, False()),
    Case(IsZero(Zero()), x, Zero(), x, Zero()),
    Case(If(True(), True(), Zero()), x, True(), y, True()),

    // t1 amd t2 have to be of same type
    Case(Inr(True(), Sum(Nat(), Bool())), x, True(), x, Zero()),
    Case(Inl(Zero(), Sum(Nat(), Nat())), y, IsZero(Zero()), z, Zero()))

  val dontTypeCheckStrings: List[String] = dontTypeCheck map { _.toString }

  val typeToStringCases: List[Tuple2[Term, String]] = List(
    // + has same precedence as *
    (Sum(Bool(), Product(Nat(), Sum(Bool(), Nat()))), "Bool+Nat*Bool+Nat"),
    (Sum(Bool(), Sum(Product(Nat(), Bool()), Nat())), "Bool+(Nat*Bool)+Nat"),
    (Function(Sum(Bool(), Nat()), Product(Nat(), Bool())), "Bool+Nat->Nat*Bool"),
    (Sum(Bool(), Sum(Function(Nat(), Nat()), Bool())), "Bool+(Nat->Nat)+Bool"),
    (Sum(Bool(), Product(Function(Nat(), Nat()), Bool())), "Bool+(Nat->Nat)*Bool"))

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
