package fos.test.helpers

object ttools {

  import fos.{ Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Pair, First, Second, Inl, Inr, Case }
  import fos.{ Type, Bool, Nat, Function, Product, Sum }
  import fos.test.helpers.Helper._

  // These expressions use as few parentheses as possible
  // Since there is no ambiguity in the grammar,
  // we have a one-to-one correspondence between AST <=> canonical Strings
  val canonicalCases = Map[String, Term](
    """false""" -> False(),
    """true""" -> True(),
    """if a then b else c""" -> If(a, b, c),
    """0""" -> Zero(),
    """succ succ 0""" -> two,
    """succ succ succ 0""" -> three,
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

    """\x:Bool.t""" -> Abs(x, Bool(), t),
    """\x:Bool->Nat.t""" -> Abs(x, Function(Bool(), Nat()), t),
    """\y:Nat->Nat.succ y y""" -> Abs(y, Function(Nat(), Nat()), Succ(App(y, y))),
    """\y:Nat->Nat.succ y y z""" -> Abs(y, Function(Nat(), Nat()), Succ(App(App(y, y), z))),
    """(\y:Nat->Nat.succ y) y""" -> App(Abs(y, Function(Nat(), Nat()), Succ(y)), y),
    """(\y:Nat.succ y) ((\x:Bool.succ 0) true)""" -> App(Abs(y, Nat(), Succ(y)), App(Abs(x, Bool(), one), True())),
    """\x:Nat.x \y:Bool.y""" -> Abs(x, Nat(), App(x, Abs(y, Bool(), y))),

    """iszero 0""" -> IsZero(Zero()),
    """iszero \x:Nat.x""" -> IsZero(Abs(x, Nat(), x)),
    """succ \x:Nat.iszero \y:Bool.y""" -> Succ(Abs(x, Nat(), IsZero(Abs(y, Bool(), y)))),
    """(succ \x:Nat.x) y""" -> App(Succ(Abs(x, Nat(), x)), y),
    """succ pred x""" -> Succ(Pred(x)),

    """if succ x then pred y else iszero 0""" -> If(Succ(x), Pred(y), IsZero(Zero())),

    """fst {x,y}""" -> First(Pair(x, y)),
    """snd {x,y}""" -> Second(Pair(x, y)),

    """pred x y fst a b""" -> Pred(App(App(x, y), First(App(a, b)))),

    """(fst {snd {succ succ 0,succ succ succ 0},\x:Bool.fst {x,x}}) a""" -> App(First(Pair(Second(Pair(two, three)), Abs(x, Bool(), First(Pair(x, x))))), a),
    """fst {snd {succ succ 0,succ succ succ 0},\x:Bool.fst {x,x}} a""" -> First(App(Pair(Second(Pair(two, three)), Abs(x, Bool(), First(Pair(x, x)))), a)),

    """{x,y}""" -> Pair(x, y),
    """{{a,b},{x,y}}""" -> Pair(Pair(a, b), Pair(x, y)),
    """{\x:Nat.x,\y:Bool.y}""" -> Pair(Abs(x, Nat(), x), Abs(y, Bool(), y)),
    """{\x:Nat.iszero x,\x:Bool.if x then 0 else succ 0}""" -> Pair(Abs(x, Nat(), IsZero(x)), Abs(x, Bool(), If(x, Zero(), one))))

  val typeCanonicalCases = Map[Term, String](
    Bool() -> "Bool",
    Nat() -> "Nat",
    Function(Nat(), Nat()) -> "Nat->Nat",
    Product(Bool(), Nat()) -> "Bool*Nat",

    // Function is right associative
    Function(Nat(), Function(Nat(), Nat())) -> "Nat->Nat->Nat",
    Function(Function(Nat(), Nat()), Nat()) -> "(Nat->Nat)->Nat",

    Function(Nat(), Function(Bool(), Function(Nat(), Bool()))) -> "Nat->Bool->Nat->Bool",
    Function(Function(Function(Nat(), Bool()), Nat()), Bool()) -> "((Nat->Bool)->Nat)->Bool",

    Function(Nat(), Function(Function(Bool(), Nat()), Bool())) -> "Nat->(Bool->Nat)->Bool",
    Function(Function(Nat(), Function(Bool(), Nat())), Bool()) -> "(Nat->Bool->Nat)->Bool",
    Function(Function(Nat(), Bool()), Function(Nat(), Bool())) -> "(Nat->Bool)->Nat->Bool",

    // Product is right associative
    Product(Nat(), Product(Nat(), Nat())) -> "Nat*Nat*Nat",
    Product(Product(Nat(), Nat()), Nat()) -> "(Nat*Nat)*Nat",

    Product(Nat(), Product(Bool(), Product(Nat(), Bool()))) -> "Nat*Bool*Nat*Bool",
    Product(Product(Product(Nat(), Bool()), Nat()), Bool()) -> "((Nat*Bool)*Nat)*Bool",

    Product(Nat(), Product(Product(Bool(), Nat()), Bool())) -> "Nat*(Bool*Nat)*Bool",
    Product(Product(Nat(), Product(Bool(), Nat())), Bool()) -> "(Nat*Bool*Nat)*Bool",
    Product(Product(Nat(), Bool()), Product(Nat(), Bool())) -> "(Nat*Bool)*Nat*Bool",

    // Product takes precedence over Function
    Function(Product(Nat(), Nat()), Bool()) -> "Nat*Nat->Bool",
    Function(Nat(), Product(Nat(), Bool())) -> "Nat->Nat*Bool",
    Function(Nat(), Function(Product(Bool(), Bool()), Nat())) -> "Nat->Bool*Bool->Nat",
    Function(Product(Function(Nat(), Bool()), Bool()), Nat()) -> "(Nat->Bool)*Bool->Nat",
    Function(Function(Product(Bool(), Bool()), Nat()), Nat()) -> "(Bool*Bool->Nat)->Nat",
    Function(Bool(), Function(Product(Bool(), Product(Nat(), Nat())), Bool())) -> "Bool->Bool*Nat*Nat->Bool",
    Product(Function(Bool(), Bool()), Product(Nat(), Function(Nat(), Bool()))) -> "(Bool->Bool)*Nat*(Nat->Bool)",
    Function(Product(Function(Bool(), Bool()), Product(Nat(), Nat())), Bool()) -> "(Bool->Bool)*Nat*Nat->Bool",
    Function(Product(Bool(), Nat()), Product(Product(Bool(), Nat()), Bool())) -> "Bool*Nat->(Bool*Nat)*Bool",
    Product(Function(Bool(), Nat()), Function(Bool(), Nat())) -> "(Bool->Nat)*(Bool->Nat)")

  def toRawString(t: Term): String = t match {
    case Product(fst, snd) => "[" + toRawString(fst) + "*" + toRawString(snd) + "]"
    case Function(i, o) => "[" + toRawString(i) + "->" + toRawString(o) + "]"
    case Nat() => "Nat"
    case Bool() => "Bool"
    case Second(p) => "snd(" + toRawString(p) + ")"
    case First(p) => "fst(" + toRawString(p) + ")"
    case Pair(fst, snd) => "pair(" + toRawString(fst) + ", " + toRawString(snd) + ")"
    case If(cond, zen, elze) => "if(" + toRawString(cond) + ", " + toRawString(zen) + ", " + toRawString(elze) + ")"
    case IsZero(t) => "iszero " + toRawString(t)
    case Pred(t) => "pred " + toRawString(t)
    case Succ(t) => "succ(" + toRawString(t) + ")"
    case App(t1, t2) => "App(" + toRawString(t1) + ", " + toRawString(t2) + ")"
    case Abs(x, typ, body) => "Abs(" + toRawString(x) + ":" + toRawString(typ) + ", " + toRawString(body) + ")"
    case Var(name) => name
    case Zero() => "0"
    case False() => "false"
    case True() => "true"

    case Sum(typ1, typ2) => "[" + toRawString(typ1) + "+" + toRawString(typ2) + "]"
    case Inl(t, typ) => "(inl " + toRawString(t) + " as " + toRawString(typ) + ")"
    case Inr(t, typ) => "(inr " + toRawString(t) + " as " + toRawString(typ) + ")"
    case Case(t, inlVar, inlTerm, inrVar, inrTerm) =>
      "(case " + toRawString(t) + " of inl " + toRawString(inlVar) + "=>" + toRawString(inlTerm) + " | inr " +
        toRawString(inrVar) + "=>" + toRawString(inrTerm) + ")"

    case _ => throw new NotImplementedError()
  }

}