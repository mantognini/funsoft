package fos

import org.scalatest._

class ParserPrinterTest extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._

  behavior of "The Parser and Printer"

  def testASTPrinting(ast: Term, expr: String) = ast.toString shouldEqual expr

  // Test input => parse => print
  val tests = Map[String, String](
    """x""" -> """x""")

  tests.foreach {
    case (in, out) => it should "properly parse <" + in + "> and print <" + out + ">" in {
      testASTPrinting(Untyped.parseOrDie(in), out)
    }
  }

}
