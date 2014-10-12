package fos

import org.scalatest._

class ParserTest extends WordSpec with Matchers {

  import fos.test.helpers.Shortcuts._

  "The parser" should {
    val correctCases = Map[String, Term](
      // WITHOUT any variable name substitution
      """x""" -> x,
      """y""" -> y,
      """xya""" -> Var("xya"),
      """x1""" -> Var("x1"),

      """x y""" -> App(x, y),
      """x y z""" -> App(App(x, y), z),
      """x y z f""" -> App(App(App(x, y), z), f),
      """x y z f g""" -> App(App(App(App(x, y), z), f), g),

      """\y. y""" -> Abs(y, y),
      """\x. x y z""" -> Abs(x, App(App(x, y), z)),
      """\x. (x y z)""" -> Abs(x, App(App(x, y), z)),
      """\x. (x y) z""" -> Abs(x, App(App(x, y), z)),
      """\x. x (y z)""" -> Abs(x, App(x, App(y, z))),
      """\y. \x. x y""" -> Abs(y, Abs(x, App(x, y))),
      """\x. \y. x y x""" -> Abs(x, Abs(y, (App(App(x, y), x)))), // λx. (λy. ((x y) x))

      """(\x. x) y z""" -> App(App(Abs(x, x), y), z),
      """(\x. x) \y.y z""" -> App(Abs(x, x), Abs(y, App(y, z))),
      """(\x. x) y \z. z""" -> App(App(Abs(x, x), y), Abs(z, z)),

      """(\x. x) (y z)""" -> App(Abs(x, x), App(y, z)),
      """(\x. x) (\y. y z)""" -> App(Abs(x, x), Abs(y, App(y, z))),
      """(\x. x) ((\y. y) z)""" -> App(Abs(x, x), App(Abs(y, y), z)),
      """(\x. x) ((\y. y) \z. z)""" -> App(Abs(x, x), App(Abs(y, y), Abs(z, z))),
      """(\x. x) (\y. y \z. z)""" -> App(Abs(x, x), Abs(y, App(y, Abs(z, z)))),
      """(\x. x) \y. y \z. z""" -> App(Abs(x, x), Abs(y, App(y, Abs(z, z)))),

      """(x)""" -> x,
      """(x y)""" -> App(x, y),
      """\f. f (f succ0)""" -> Abs(f, App(f, App(f, Var("succ0")))),
      """\f. f (f (succ0))""" -> Abs(f, App(f, App(f, Var("succ0")))),

      """(\f. f (f (g z))) (\x. g (g (g x)))""" -> App(Abs(f, App(f, App(f, App(g, z)))), Abs(x, App(g, App(g, App(g, x))))),
      """(\x. \y. x y) (\z. z) f g """ -> App(App(App(Abs(x, Abs(y, App(x, y))), Abs(z, z)), f), g))

    // TODO add incorrect cases and catch exception

    correctCases.foreach {
      case (input, ast) => "procude the correct AST with input " + input in {
        try {
          val res = Untyped.parseOrDie(input)
          assert(res.toRawString === ast.toRawString)
        } catch {
          case Untyped.ParseException(e) => fail(e)
        }
      }
    }
  }

}
