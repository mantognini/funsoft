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
      """\y.y""" -> Abs(y, y),
      """\y.\x. x y""" -> Abs(y, Abs(x, App(x, y))),
      """\x. \y. x y x""" -> Abs(x, Abs(y, (App(App(x, y), x)))), // λx. (λy. ((x y) x))
      """\x. x y z""" -> Abs(x, App(App(x, y), z)),
      """\x. (x y z)""" -> Abs(x, App(App(x, y), z)),
      """\x. (x y) z""" -> Abs(x, App(App(x, y), z)),
      """\x. x (y z)""" -> Abs(x, App(x, App(y, z))),
      """\f.f(f(succ0))""" -> Abs(f, App(f, App(f, Var("succ0")))),
      """\f.f (f succ0)""" -> Abs(f, App(f, App(f, Var("succ0")))),
      """(\f. f (f (g z))) (\x. g (g (g x)))""" -> App(Abs(f, App(f, App(f, App(g, z)))), Abs(x, App(g, App(g, App(g, x))))),
      """(x)""" -> x,
      """(x y)""" -> App(x, y),
      """(\x. \y. x y) (\z. z) f g """ -> App(App(App(Abs(x, Abs(y, App(x, y))), Abs(z, z)), f), g))

    // TODO add incorrect cases and catch exception

    correctCases.foreach {
      case (input, ast) => "procude the correct AST with input " + input in {
        try {
          val res = Untyped.parseOrDie(input)
          assert(res === ast)
        } catch {
          case Untyped.ParseException(e) => fail(e)
        }
      }
    }
  }

}
