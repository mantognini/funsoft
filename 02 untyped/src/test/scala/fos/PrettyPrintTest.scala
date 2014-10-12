package fos

import org.scalatest._

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._

  behavior of "The pretty printer"

  val termsAndStringsMap = ttools.canonicalCases map { _.swap }

  // Test AST printing
  termsAndStringsMap.foreach {
    case (ast, expr) => it should "properly print " + expr in {
      ast.toString shouldEqual expr
    }
  }

}
