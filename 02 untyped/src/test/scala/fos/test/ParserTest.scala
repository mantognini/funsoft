package fos.test

import org.scalatest._

class ParserTest extends WordSpec with Matchers {

  import fos.test.helpers.ttools
  import fos.Untyped

  "The parser" should {
    ttools.correctCases.foreach {
      case (input, ast) => "procude the correct AST with input " + input in {
        try {
          val res = Untyped.parseOrDie(input)
          assert(res.toRawString === ast.toRawString)
        } catch {
          case Untyped.ParseException(e) => fail(e)
        }
      }
    }
  }

}
