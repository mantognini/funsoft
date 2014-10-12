package fos

import org.scalatest.Exceptional
import scala.collection.immutable.List
import scala.collection.immutable.Map

object ttools {

  import fos.test.helpers.Shortcuts._

  // These expression use the less ()'s possible
  // Since there is no ambiguity in the grammar,
  // we have one-to-one correspondence between AST <=> canonical Strings
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
      App(App(App(x, y), z), f)),

    """\y. y""" -> Abs(y, y),
    """\x. x y z""" -> Abs(x, App(App(x, y), z)),
    """\x. x y z f""" -> Abs(x, App(App(App(x, y), z), f)),
    """\x. x y z f g""" -> Abs(x, App(App(App(App(x, y), z), f), g)),

    """\y. \x. x y""" -> Abs(y, Abs(x, App(x, y))),
    """\x. \y. x y x""" -> Abs(x, Abs(y, (App(App(x, y), x)))), // λx. (λy. ((x y) x))

    """(\x. x) y z""" -> App(App(Abs(x, x), y), z),
    """(\x. x) \y. y z""" -> App(Abs(x, x), Abs(y, App(y, z))),
    """(\x. x) y \z. z""" -> App(App(Abs(x, x), y), Abs(z, z)),

    """(\x. x) ((\y. y) z)""" -> App(Abs(x, x), App(Abs(y, y), z)),
    """(\x. x) ((\y. y) \z. z)""" -> App(Abs(x, x), App(Abs(y, y), Abs(z, z))),
    """(\x. x) (y z)""" -> App(Abs(x, x), App(y, z)),

    """(\x. \y. x y) (\z. z) f g""" -> App(App(App(Abs(x, Abs(y, App(x, y))), Abs(z, z)), f), g),
    """(\f. f f g z) \x. g g g x""" -> App(Abs(f, App(App(App(f, f), g), z)),
      Abs(x, App(App(App(g, g), g), x))),
    """(\f. f f g z y) \x. g g g x y""" -> App(Abs(f, App(App(App(App(f, f), g), z), y)),
      Abs(x, App(App(App(App(g, g), g), x), y))))
  // These string are given with extra ()'s, but still well formated
  val wellFormatedCases = Map[String, Term](
    """\x. (x y z)""" -> Abs(x, App(App(x, y), z)),
    """\x. (x y) z""" -> Abs(x, App(App(x, y), z)),
    """\x. x (y z)""" -> Abs(x, App(x, App(y, z))),
    """(\x. x) (\y. y z)""" -> App(Abs(x, x), Abs(y, App(y, z))),
    """(\x. x) (\y. y \z. z)""" -> App(Abs(x, x), Abs(y, App(y, Abs(z, z)))),
    """(\x. x) \y. y \z. z""" -> App(Abs(x, x), Abs(y, App(y, Abs(z, z)))),

    """(x)""" -> x,
    """(x y)""" -> App(x, y),
    """\f. f (f succ0)""" -> Abs(f, App(f, App(f, Var("succ0")))),
    """\f. f (f (succ0))""" -> Abs(f, App(f, App(f, Var("succ0")))),

    """(\f. f (f (g z))) (\x. g (g (g x)))""" -> App(Abs(f, App(f, App(f, App(g, z)))), Abs(x, App(g, App(g, App(g, x))))),

    """(\f. f f g z) (\x. g g g x)""" -> App(Abs(f, App(App(App(f, f), g), z)), Abs(x, App(App(App(g, g), g), x))))

  val correctCases = canonicalCases ++ wellFormatedCases

}