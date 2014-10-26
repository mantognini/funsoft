package fos.test

import org.scalatest._

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.test.helpers.ttools

  behavior of "The pretty printer"

  val termsToString = ttools.canonicalCases map { _.swap }

  termsToString.foreach {
    case (ast, expr) => it should "properly print " + expr + ". NB, AST is " + ast.toRawString in {
      ast.toString shouldEqual expr
    }
  }

  // TODO: Implement these type tests
}