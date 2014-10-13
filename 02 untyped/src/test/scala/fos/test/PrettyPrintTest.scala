package fos.test

import org.scalatest._

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._
  import fos.test.helpers.ttools

  behavior of "The pretty printer"

  val termsAndStringsMap = ttools.canonicalCases map { _.swap }

  // Test AST printing
  termsAndStringsMap.foreach {
    case (ast, expr) => it should "properly print " + expr + ". NB, AST is " + ast.toRawString in {
      ast.toString shouldEqual expr
    }
  }

}
