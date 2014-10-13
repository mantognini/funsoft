package fos.test

import org.scalatest._

class ReduceCallByValueTest extends FlatSpec with Matchers with GivenWhenThen {

  import fos.{ Untyped, Term, App, Abs, Var }
  import fos.test.helpers.Shortcuts._
  import Untyped.NoRuleApplies

  //  \x.x ((\x.\f.(\z.z) \g.g) y) =>
  //\x.x ((\x.\f.\g.g) y) => Stuck
  //
  //(1) \x.x ((\x.\f.(\z.z) x) \y.y) =>
  //(2) \x.x (\f.(\z.z) \y.y) =>
  //(3) \f.(\z.z) \y.y =>
  //(4) \f.\y.y => Stuck
  //
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
    // Statement example
    """\y. (\x. x) y""" :: Nil,

    /// Non-normal forms
    // from TAPL p.57
    """(\x. x) ((\x. x) (\z. (\x. x) z))""" :: """(\x. x) (\z. (\x. x) z)""" :: """\z. (\x. x) z""" :: Nil,

    List())

  def parse(input: String) = Untyped.parseOrDie(input)

  def reduceOnce(program: Term) = Untyped.reduceCallByValue(program)

  def testNormal(input: String) {
    When("the input is a normal form such as <" + input + ">")

    a[NoRuleApplies] should be thrownBy {
      val output = reduceOnce(parse(input))
      info("an exception was expected - instead the input was reduced to " + output)
    }

    info("🍻")
  }

  def testSteps(steps: List[String]) {
    // reverse the order to perform the test from normal to initial form
    val rsteps = steps.reverse

    testNormal(rsteps.head)

    rsteps reduceLeft { (reduced: String, initial: String) =>
      When("the input is a non-normal form such as <" + initial + "> which reduce into <" + reduced + ">")

      val i = reduceOnce(parse(initial)).toString
      val r = parse(reduced).toString

      i shouldBe r

      info("🍺")

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

  it should "properly reduce terms" in {
    cbvrCasesWhichTerminate foreach test
  }

}
