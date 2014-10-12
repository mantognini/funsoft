package fos

import org.scalatest._

class PrinterConsistency extends FlatSpec with Matchers {

  import fos.test.helpers.Shortcuts._

  behavior of "Term.toString"

  // This test assumes the parser p ("p(String): Term") does its job correctly
  // .. no variable re-naming ..
  // It tests the consistency of the Term.toString methods. 
  // i.e. : for all term t: p(t.toString) == t
  val tests = List(
    x,
    App(x, y),
    App(x, App(y, z)),
    App(App(x, y), z),
    App(Abs(x, x), App(y, z)),
    Abs(x, x),
    Abs(x, Abs(y, App(x, y))),
    Abs(x, App(Abs(y, App(y, z)), y)),
    App(App(Abs(x, Abs(f, x)), y), z),
    App(x, Abs(y, y)),
    App(Abs(x, x), Abs(y, y)),
    App(App(z, Abs(x, x)), Abs(y, y)),
    App(App(Abs(z, z), Abs(x, x)), Abs(y, y)),
    App(Abs(g, App(g, Abs(x, x))), Abs(f, f)),
    App(App(Abs(x, Abs(y, App(x, y))), f), g),
    App(App(App(Abs(x, Abs(y, App(x, y))), Abs(z, z)), f), g),
    App(Abs(x, Abs(y, App(App(z, x), y))), Abs(f, f)))

  tests.foreach { el =>
    it should "be consistent with " + el in {
      try {
        val res = Untyped.parseOrDie(el.toString)
        assert(el == res)
      } catch {
        case Untyped.ParseException(e) => fail(e)
      }
    }
  }

}
