package fos

import org.scalatest._

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._

  behavior of "The pretty printer"

  val tests = Map[Term, String](
    x -> """x""",
    App(x, y) -> """x y""",
    App(x, App(y, z)) -> """x y z""",
    App(App(x, y), z) -> """(x y) z""",
    Abs(x, x) -> """\x. x""",
    Abs(x, Abs(y, App(x, y))) -> """\x. \y. x y""",
    Abs(x, App(Abs(y, App(y, z)), y)) -> """\x. (\y. z y) y""",
    App(App(Abs(x, Abs(f, x)), y), z) -> """(\x. \f. x) y z""",
    App(Abs(x, Abs(y, App(App(z, x), y))), Abs(x, x)) -> """(\x. \y. (z x) y) (\x. x)""")

  // TODO Add more test

  tests.foreach {
    case (ast, expr) => it should "properly print " + expr in {
      ast.toString shouldEqual expr
    }
  }

}
