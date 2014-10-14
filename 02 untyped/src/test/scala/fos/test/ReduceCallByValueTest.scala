package fos.test

import org.scalatest._

class ReduceCallByValueTest extends FlatSpec with Matchers with GivenWhenThen {

  import fos.{ Untyped, Term, App, Abs, Var }
  import fos.test.helpers.Shortcuts._
  import Untyped.NoRuleApplies

  //Following Church, a term of the form (\x.t12) t2 is called a redex
  //
  //only outermost redex are reduced and where a redex is reduced only
  //when its right-hand side has already been reduced to a value

  // cbvr == call by value reduction
  // A list of < sequence of reduction > which itself is represented
  // by a < list of terms > each reducing to the next term in the list.
  // Hence, last element of the list cannot be reduced
  // Note: 	We assume that the parser and prettyPrinter are correct
  // 		and we write the terms with strings for readability
  val cbvrCasesWhichTerminate: List[List[String]] = List(
    /// Normal forms
    """\x. x""" :: Nil,
    """\x. \y. y""" :: Nil,
    """\x. \y. x y""" :: Nil,
    """x""" :: Nil,
    """x y""" :: Nil,
    """x y z""" :: Nil,
    """x y z a""" :: Nil,
    """x y z a b""" :: Nil,
    """x x x x x y y y z""" :: Nil,
    """(\x. x) y""" :: Nil, // yes, y is NOT a value.
    """z (\x.x) (\y.y)""" :: Nil,
    """\x.x ((\x.\f.(\z.z) x) \y.y)""" :: Nil,
    // Statement example
    """\y. (\x. x) y""" :: Nil,

    /// Non-normal forms
    """(\x. x x) \y. y""" :: """(\y. y) \y. y""" :: """\y.y""" :: Nil,
    """(\x. x z) (\y. y)""" :: """(\y. y) z""" :: Nil,
    """(\x. x z) (\y. y) (\z. z)""" :: """((\y. y) z) (\z. z)""" :: Nil,
    """(\x.y x) (\z.z)""" :: """y \z.z""" :: Nil,
    """(\x.x) ((\x.\f.(\z.z) x) \y.y)""" :: """(\x. x) (\f.(\z.z) \y.y)""" :: """\f. (\z.z) \y.y""" :: Nil,
    """((\x. x) (\y. y)) \z. z""" :: """(\y. y) \z. z""" :: """\z.z""" :: Nil,
    """((\x. x) (\y. y)) ((\f. f) (\g. g))""" :: """(\y. y) ((\f. f) (\g. g))""" :: """(\y. y) (\g. g)""" :: """(\g. g)""" :: Nil,

    /// Assert there's no name substitution
    """(\x. x y z) (\y. x y z)""" :: """(\y. x y z) y z""" :: Nil,
    """(\x. \y. x y z) (\y. x y z)""" :: """\y. (\y. x y z) y z""" :: Nil,

    // from TAPL p.57
    """(\x. x) ((\x. x) (\z. (\x. x) z))""" :: """(\x. x) (\z. (\x. x) z)""" :: """\z. (\x. x) z""" :: Nil,
    Nil)

  def parse(input: String) = Untyped.parseOrDie(input)

  def reduceOnce(program: Term) = Untyped.reduceCallByValue(program)

  var testId = 0;

  def testNormal(input: String) {
    testId = testId + 1
    it should "properly stop on normal terms (id:" + testId + ")" in {
      When("the input is a normal form such as <" + input + ">")

      a[NoRuleApplies] should be thrownBy {
        val output = reduceOnce(parse(input))
        info("an exception was expected - instead the input was reduced to " + output)
      }

      info("🍻")
    }
  }

  def compare(initial: String, reduced: String) {
    val pi = parse(initial)
    val i = reduceOnce(pi)
    val r = parse(reduced)

    info("pi is " + pi)
    info("i  is " + i)
    info("r  is " + r)

    i shouldBe r

    info("🍺")
  }

  def testSteps(steps: List[String]) {
    // reverse the order to perform the test from normal to initial form
    val rsteps = steps.reverse

    testNormal(rsteps.head)

    rsteps reduceLeft { (reduced: String, initial: String) =>
      testId = testId + 1
      it should "properly reduce terms (id:" + testId + ")" in {
        When("the input is a non-normal form such as <" + initial + "> which reduce into <" + reduced + ">")

        compare(initial, reduced)
      }

      initial // is the reduced of the next test
    }
  }

  def test(steps: List[String]) {
    steps match {
      // skip empty
      case Nil =>

      // normal form
      case s :: Nil => testNormal(s)

      // multiple steps
      case ss => testSteps(ss)
    }
  }

  behavior of "The call-by-value strategy"

  cbvrCasesWhichTerminate foreach test

  it should "properly loop forever" in {
    val omega = """(\x. x x) (\x. x x)"""
    compare(omega, omega)
  }

}
