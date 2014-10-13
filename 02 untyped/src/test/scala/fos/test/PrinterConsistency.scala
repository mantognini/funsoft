package fos.test

import org.scalatest._

class PrinterConsistency extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._
  import fos.test.helpers.ttools
  import fos.Untyped

  behavior of "Term.toString"

  // This test assumes the parser p ("p(String): Term") does its job correctly
  // .. no variable re-naming ..
  // It tests the consistency of the Term.toString methods. 
  // i.e. for all term t: p(t.toString) == t
  ttools.correctCases.values.toList.removeDuplicates.foreach { el =>
    it should "be consistent with " + el.toRawString in {
      try {
        val res = Untyped.parseOrDie(el.toString)
        assert(el == res)
      } catch {
        case Untyped.ParseException(e) => fail(e)
      }
    }
  }

}
