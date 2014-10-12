package fos

import org.scalatest._

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._

  behavior of "The pretty printer"

  // Test AST printing
  val tests = Map[Term, String](
    // WITHOUT any variable name substitution
    x -> """x""",
    App(x, y) -> """x y""",
    App(x, App(y, z)) -> """x y z""", // TODO: FALSE! it should produce << x (y z) >> !!! Application is left associative!
    App(App(x, y), z) -> """(x y) z""", // TODO: better is x y z
    Abs(x, x) -> """\x. x""",
    Abs(x, Abs(y, App(x, y))) -> """\x. \y. x y""",
    Abs(x, App(Abs(y, App(y, z)), y)) -> """\x. (\y. y z) y""",
    App(App(Abs(x, Abs(f, x)), y), z) -> """(\x. \f. x) y z""",
    App(x, Abs(y, y)) -> """x (\y. y)""", // TODO: better is "x \y y"
    App(Abs(x, x), Abs(y, y)) -> """(\x. x) (\y. y)""", // TODO: better is (\x. x) \y. y
    App(App(z, Abs(x, x)), Abs(y, y)) -> """z (\x. x) (\y. y)""", // TODO: better is z (\x. x) \y. y
    // TODO: better is (\z. z) (\x. x) \y. y
    App(App(Abs(z, z), Abs(x, x)), Abs(y, y)) -> """(\z. z) (\x. x) (\y. y)""",

    // TODO: better is (\g. g \x. x) \f. f
    // Fail-info[improvement]: should produce (\g. g \x. x) (\f. f)
    // but produce (\g. g (\x. x)) (\f. f) 
    App(Abs(g, App(g, Abs(x, x))), Abs(f, f)) -> """(\g. g \x. x) (\f. f)""",

    App(App(Abs(x, Abs(y, App(x, y))), f), g) -> """(\x. \y. x y) f g""",

    // Fail-info[improvement]: This test is failing, should produce (\x. \y. x y) (\z. z) f g
    // but produce ((\x. \y. x y) (\z. z) f) g
    App(App(App(Abs(x, Abs(y, App(x, y))), Abs(z, z)), f), g) -> """(\x. \y. x y) (\z. z) f g""",

    // TODO: better is (\x. \y. z x y) \f. f
    App(Abs(x, Abs(y, App(App(z, x), y))), Abs(f, f)) -> """(\x. \y. (z x) y) (\f. f)""")

  tests.foreach {
    case (ast, expr) => it should "properly print " + expr in {
      ast.toString shouldEqual expr
    }
  }

}
