package fos

import org.scalatest._

class ParserPrinterTest extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._

  behavior of "The Parser and Printer"

  def testASTPrinting(ast: Term, expr: String) = ast.toString shouldEqual expr

  // Test input => parse => print
  val tests = Map[String, String](
    // WITHOUT any variable name substitution
    // Base case
    """x""" -> """x""",
    """x y""" -> """x y""",
    """\x. x""" -> """\x. x""",
    """y \x. x""" -> """y \x. x""",
    """\x. x y""" -> """\x. x y""",
    """z \x. x y""" -> """z \x. x y""",
    """(\x. x) y""" -> """(\x. x) y""",
    // Remove extra parentheses
    """(x)""" -> """x""",
    """((x y))""" -> """x y""",
    """((\x. (x))) ((y z))""" -> """(\x. x) (y z)""")

  // TODO add more tests

  tests.foreach {
    case (in, out) => it should "properly parse <" + in + "> and print <" + out + ">" in {
      testASTPrinting(Untyped.parseOrDie(in), out)
    }
  }

}
