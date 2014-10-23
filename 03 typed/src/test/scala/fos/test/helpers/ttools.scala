package fos.test.helpers

object ttools {

  import fos.{ SimplyTyped, Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Bool, Nat, Function, Product }
  import fos.test.helpers.Helper._

  // These expressions use as few parentheses as possible
  // Since there is no ambiguity in the grammar,
  // we have a one-to-one correspondence between AST <=> canonical Strings
  val canonicalCases = Map[String, Term](
    """false""" -> False,
    """true""" -> True,
    """if a then b else c""" -> If(a, b, c),
    """0""" -> Zero,
    """2""" -> Succ(Succ(Zero)),
    """3""" -> Succ(Succ(Succ(Zero))),
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

    """\x:Bool.t""" -> Abs(x, Bool, t),
    """\x:Bool->Nat.t""" -> Abs(x, Function(Bool, Nat), t),
    """\y:Nat->Nat.succ y y""" -> Abs(y, Function(Nat, Nat), App(Succ(y), y)),
    """\y:Nat->Nat.succ y y z""" -> Abs(y, Function(Nat, Nat), App(App(Succ(y), y), z)),
    """(\y:Nat->Nat.succ y) y""" -> App(Abs(y, Function(Nat, Nat), Succ(y)), y),
    """(\y:Nat.succ y) ((\x:Bool.1) true)""" -> App(Abs(y, Nat, Succ(y)), App(Abs(x, Bool, Succ(Zero)), True)),
    """\x:Nat.x \y:Bool.y""" -> Abs(x, Nat, App(x, Abs(y, Bool, y))) //
    //
    //
    )

  val prettyPrintCases = Map[String, Term](
    """pred(Zero)""" -> Pred(Zero),
    """pred(pred(Zero))""" -> Pred(Pred(Zero)),
    """pred(succ(Zero))""" -> Pred(Succ(Zero)))

}