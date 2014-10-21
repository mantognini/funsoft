package fos.test.helpers

object ttools {

  import fos.{ SimplyTyped, Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Bool, Nat, Function, Product }
  import fos.test.helpers.Helper._

  // These expressions use as few parentheses as possible
  // Since there is no ambiguity in the grammar,
  // we have a one-to-one correspondence between AST <=> canonical Strings
  val canonicalCases = Map[String, Term](
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
      App(App(App(x, y), z), f)))

}