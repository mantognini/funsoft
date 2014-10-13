package fos.test

import org.scalatest._

class SubstituteTest extends FlatSpec with Matchers {

  import fos.{ Untyped, Term }

  behavior of "The Substitution"

  def testASTPrinting(ast: Term, expr: String) = ast.toString shouldEqual expr

  def parse(input: String) = Untyped.parseOrDie(input)

  def reduceOnce(tree: Term) = try { Untyped.reduceNormalOrder(tree) } catch { case e: Untyped.NoRuleApplies => tree }

  // Those tests are for normal reduce applied once
  val tests = List(
    // WITHOUT any variable name substitution
    "x" -> "x",
    "x y" -> "x y",
    """(\x. x) y""" -> "y",
    """(\x. x y) z""" -> "z y",
    """(\x. x) (y z)""" -> "y z",
    """(\x. x) (\y. y)""" -> """\y. y""",
    """(\x. x) \y. y""" -> """\y. y""",
    """(\x. \y. x y) a b""" -> """(\y. a y) b""",

    /// WITH potential variable name substitution
    """(\x. x) x""" -> """x""",
    """(\x. x) (x y)""" -> """x y""",
    """(\x. \y. x y) (y z)""" -> """\y1. y z y1""",
    """(\x. \y. \z. x y z) (y z)""" -> """\y1. \z1. y z y1 z1""",
    """(\x. \y. x) (\x. \y. x y)""" -> """\y. \x. \y. x y""",
    """(\y. \x. \y. x y) z""" -> """\x. \y. x y""",
    """(\x. \y. x y) (\y. y)""" -> """\y. (\y. y) y""",
    """\y. (\y. y) y""" -> """\y. y""",
    """\x. (\y. x y) y""" -> """\x. x y""",
    """(\x. \y. \z. x y z)(x y z)""" -> """\y1. \z1. x y z y1 z1""",
    """(\x. \y. \z. x y z z1)(x y z)""" -> """\y1. \z2. x y z y1 z2 z1""",
    """(\x. \y. \z. x y z y1)(x y z)""" -> """\y2. \z1. x y z y2 z1 y1""",
    """(\x. \y. \z. x y z y1 z1)(x y z)""" -> """\y2. \z2. x y z y2 z2 y1 z1""",
    """(\x. \y. \z. x y z y1 z2)(x y z)""" -> """\y2. \z1. x y z y2 z1 y1 z2""",
    """(\x. \y. \z. x y z y1 y2 y3 z1 z3)(x y z)""" -> """\y4. \z4. x y4 z2 y1 y2 y3 z1 z4""")

  tests.foreach {
    case (input, expected) => it should "properly reduce <" + input + "> into <" + expected + ">" in {
      val tree = parse(input)
      val after = reduceOnce(tree)
      testASTPrinting(after, expected)
    }
  }
}
