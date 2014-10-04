package fos

import org.scalatest._

class ParserTest extends WordSpec with Matchers {

  // A few shortcuts...
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")

  "The parser" should {
    val correctCases = Map[String, Term](
      """x""" -> x,
      """y""" -> y,
      """xya""" -> Var("xya"),
      """x1""" -> Var("x1"),
      """x y""" -> App(x, y),
      """\y.y""" -> Abs(y, y),
      """\y.\x. x y""" -> Abs(y, Abs(x, App(x, y))),
      """\x x y z""" -> Abs(x, App(App(x, y), z)),
      """\x x (y z)""" -> Abs(x, App(x, Par(App(y, z)))),
      """(x)""" -> Par(x),
      """(x y)""" -> Par(App(x, y)) // TODO add more tests
      )

    correctCases.foreach {
      case (input, ast) => "procude the correct AST with input " + input in {
        assert(Untyped.parse(input) === ast)
      }
    }
  }

}
