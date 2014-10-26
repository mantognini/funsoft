package fos.test.helpers

object ttools {

  import fos.{ SimplyTyped, Term, True, False, Zero, If, Pair, First, Second, Succ, Pred, IsZero, Var, Abs, App, Bool, Nat, Function, Product }
  import fos.test.helpers.Helper._

  // These expressions use as few parentheses as possible
  // Since there is no ambiguity in the grammar,
  // we have a one-to-one correspondence between AST <=> canonical Strings
  val canonicalCases = Map[String, Term](
    """false""" -> False,
    """true""" -> True,
    """if a then b else c""" -> If(a, b, c),
    """0""" -> Zero,
    """2""" -> two,
    """3""" -> three,
    """x""" -> x,
    """y""" -> y,
    """xya""" -> Var("xya"),
    """x1""" -> Var("x1"),

    """x y""" -> App(x, y),
    """x y z""" -> App(App(x, y), z),
    """x y z f""" -> App(App(App(x, y), z), f),
    """x y z f g""" -> App(App(App(App(x, y), z), f), g),

    """x y (f g)""" -> App(App(x, y), App(f, g)),
    """x y z (f g x)""" -> App(App(App(x, y), z), App(App(f, g), x)),
    """x y z f (g x)""" -> App(App(App(App(x, y), z), f), App(g, x)),
    """x y z f (x y z f)""" -> App(App(App(App(x, y), z), f),
      App(App(App(x, y), z), f)),

    """if a then if x then y else z else c""" -> If(a, If(x, y, z), c),
    """if if a then b else c then if x then y else y else x""" -> If(If(a, b, c), If(x, y, y), x),
    """pred succ pred x y""" -> Pred(Succ(Pred(App(x, y)))),

    """\x:Bool.t""" -> Abs(x, Bool, t),
    """\x:Bool->Nat.t""" -> Abs(x, Function(Bool, Nat), t),
    """\y:Nat->Nat.succ y y""" -> Abs(y, Function(Nat, Nat), Succ(App(y, y))),
    """\y:Nat->Nat.succ y y z""" -> Abs(y, Function(Nat, Nat), Succ(App(App(y, y), z))),
    """(\y:Nat->Nat.succ y) y""" -> App(Abs(y, Function(Nat, Nat), Succ(y)), y),
    """(\y:Nat.succ y) ((\x:Bool.1) true)""" -> App(Abs(y, Nat, Succ(y)), App(Abs(x, Bool, one), True)),
    """\x:Nat.x \y:Bool.y""" -> Abs(x, Nat, App(x, Abs(y, Bool, y))),

    """iszero 0""" -> IsZero(Zero),
    """iszero \x:Nat.x""" -> IsZero(Abs(x, Nat, x)),
    """succ \x:Nat.iszero \y:Bool.y""" -> Succ(Abs(x, Nat, IsZero(Abs(y, Bool, y)))),
    """(succ \x:Nat.x) y""" -> App(Succ(Abs(x, Nat, x)), y),
    """succ pred x""" -> Succ(Pred(x)),

    """if succ x then pred y else iszero 0""" -> If(Succ(x), Pred(y), IsZero(Zero)),

    """fst {x,y}""" -> First(Pair(x, y)),
    """snd {x,y}""" -> Second(Pair(x, y)),

    """pred x y fst a b""" -> Pred(App(App(x, y), First(App(a, b)))),

    """(fst {snd {2,3},\x:Bool.fst {x,x}}) a""" -> App(First(Pair(Second(Pair(two, three)), Abs(x, Bool, First(Pair(x, x))))), a),
    """fst {snd {2,3},\x:Bool.fst {x,x}} a""" -> First(App(Pair(Second(Pair(two, three)), Abs(x, Bool, First(Pair(x, x)))), a)),

    """{x,y}""" -> Pair(x, y),
    """{{a,b},{x,y}}""" -> Pair(Pair(a, b), Pair(x, y)),
    """{\x:Nat.x,\y:Bool.y}""" -> Pair(Abs(x, Nat, x), Abs(y, Bool, y)),
    """{\x:Nat.iszero x,\x:Bool.if x then 0 else 1}""" -> Pair(Abs(x, Nat, IsZero(x)), Abs(x, Bool, If(x, Zero, one))) //
    //
    //
    )

  val typeCanonicalCases = Map[Term, String](
    Bool -> "Bool",
    Nat -> "Nat",
    Function(Nat, Nat) -> "Nat->Nat",
    Product(Bool, Nat) -> "Bool*Nat",

    // Function is right associative
    Function(Nat, Function(Nat, Nat)) -> "Nat->Nat->Nat",
    Function(Function(Nat, Nat), Nat) -> "(Nat->Nat)->Nat",

    Function(Nat, Function(Bool, Function(Nat, Bool))) -> "Nat->Bool->Nat->Bool",
    Function(Function(Function(Nat, Bool), Nat), Bool) -> "((Nat->Bool)->Nat)->Bool",

    Function(Nat, Function(Function(Bool, Nat), Bool)) -> "Nat->(Bool->Nat)->Bool",
    Function(Function(Nat, Function(Bool, Nat)), Bool) -> "(Nat->Bool->Nat)->Bool",
    Function(Function(Nat, Bool), Function(Nat, Bool)) -> "(Nat->Bool)->Nat->Bool",

    // Product is right associative
    Product(Nat, Product(Nat, Nat)) -> "Nat*Nat*Nat",
    Product(Product(Nat, Nat), Nat) -> "(Nat*Nat)*Nat",

    Product(Nat, Function(Bool, Function(Nat, Bool))) -> "Nat*Bool*Nat*Bool",
    Product(Product(Product(Nat, Bool), Nat), Bool) -> "((Nat*Bool)*Nat)*Bool",

    Product(Nat, Product(Product(Bool, Nat), Bool)) -> "Nat*(Bool*Nat)*Bool",
    Product(Product(Nat, Product(Bool, Nat)), Bool) -> "(Nat*Bool*Nat)*Bool",
    Product(Product(Nat, Bool), Product(Nat, Bool)) -> "(Nat*Bool)*Nat*Bool",

    // Product takes precedence over Function
    Function(Product(Nat, Nat), Bool) -> "Nat*Nat->Bool",
    Function(Nat, Product(Nat, Bool)) -> "Nat->Nat*Bool",
    Function(Nat, Function(Product(Bool, Bool), Nat)) -> "Nat->Bool*Bool->Nat",
    Function(Product(Function(Nat, Bool), Bool), Nat) -> "(Nat->Bool)*Bool->Nat",
    Function(Function(Product(Bool, Bool), Nat), Nat) -> "(Bool*Bool->Nat)->Nat",
    Function(Function(Bool, Product(Bool, Product(Nat, Nat))), Bool) -> "Bool->Bool*Nat*Nat->Bool",
    Product(Function(Bool, Bool), Product(Nat, Function(Nat, Bool))) -> "(Bool->Bool)*Nat*(Nat->Bool)",
    Function(Product(Function(Bool, Bool), Product(Nat, Nat)), Bool) -> "(Bool->Bool)*Nat*Nat->Bool")

  def getListFrom(m: Map[Term, String]): List[(String, Term)] = m.foldLeft(List[(String, Term)]()) {
    case (tail, (key, value)) => (value -> key) :: tail
  }
}