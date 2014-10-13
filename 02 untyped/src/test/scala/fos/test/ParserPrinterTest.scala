package fos.test

import org.scalatest._

class ParserPrinterTest extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._
  import fos.{ Untyped, Term }

  behavior of "The Parser and Printer"

  def testASTPrinting(ast: Term, expr: String) = ast.toString shouldEqual expr

  // Test input => parse => print
  val tests = Map[String, String](
    // WITHOUT any variable name substitution
    // term without ()'s should not produce any ()'s .. (but hard to ensure, or?)
    """x""" -> """x""",
    """x y""" -> """x y""",
    """\x. x""" -> """\x. x""",
    """y \x. x""" -> """y \x. x""",
    """\x. x y""" -> """\x. x y""",
    """z \x. x y""" -> """z \x. x y""",
    """\x. \y. x y""" -> """\x. \y. x y""",
    """\s. \z. z""" -> """\s. \z. z""", // c0
    """\x.x\y.y""" -> """\x. x \y. y""",
    """\x. \y. x y \z. z x""" -> """\x. \y. x y \z. z x""",

    // terms with important ()'s that must not be removed
    """(\x. x) y""" -> """(\x. x) y""",
    """(\x. \y. x) y""" -> """(\x. \y. x) y""",
    """x (y z)""" -> """x (y z)""",

    // Should not add extra ()'s
    """(x)""" -> """x""",
    """((x y))""" -> """x y""",
    """((\x. (x))) ((y z))""" -> """(\x. x) (y z)""",

    // Should not produce useless ()'s
    """(\x. x)(\y. y)""" -> """(\x. x) \y. y""",
    """x (y)""" -> """x y""",
    """(x y) z""" -> """x y z""")

  tests.foreach {
    case (in, out) => it should "properly parse <" + in + "> and print <" + out + ">" in {
      testASTPrinting(Untyped.parseOrDie(in), out)
    }
  }

}
