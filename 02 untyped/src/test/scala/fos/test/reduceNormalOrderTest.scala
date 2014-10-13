package fos.test

import org.scalatest._
import fos.Term
import fos.{ Term, App, Abs, Var }

class reduceNormalOrderTest extends WordSpec with Matchers {

  import fos.test.helpers.Shortcuts._
  import fos.Untyped

  // nr == normal reduction
  // A list of < sequence of reduction > which itself is represented
  // by a < list of terms > each reducing to the next term in the list.
  // Hence, last element of the list cannot be reduced
  // Note: 	We assume that the parser and prettyPrinter are correct
  // 		and we write the terms with strings for readability
  val nrCasesWhichTerminate: List[List[String]] = List(
    // Examples seen somewhere
    List(
      """\y.(\x.x) y""", // Statement example
      """\y.y"""),
    List(
      """(\x.x) ((\x.x) (\z.(\x.x) z))""", // from TAPL p.56
      """(\x.x) \z.(\x.x) z""",
      """\z.(\x.x) z""",
      """\z.z"""),

    // without substitutions - base cases
    List(
      """\x.x"""),
    List(
      """\x.\y.y"""),
    List(
      """\x.\y.x y"""),
    List(
      """(\x.x) y""",
      """y"""),
    List(
      """(\x.a b) y""",
      """a b"""),
    List(
      """(\x.x x) y""",
      """y y"""),
    List(
      """x y z"""),
    List(
      """x (\y.y) z"""), // yes, z is applied to (x (\y.y)) and not only (\y.y) -> we are stuck there
    List(
      """x (\y.y) \z.z"""), // same there
    List(
      """(\x.x x) \y.y""",
      """(\y.y) \y.y""",
      """\y.y"""),
    List(
      """(\x.x z) \y.y""",
      """(\y.y) z""",
      """z"""),
    List(
      """(\x.z x) \y.y""",
      """z \y.y"""),
    List(
      """(\x.z x) \y.y \f.f""",
      """z \y.y \f.f"""),
    List(
      """(\x.x z g) \y.y \f.f""",
      """(\y.y \f.f) z g""",
      """z (\f.f) g"""),
    List(
      """(\x.x) g ((\y.y) f)""", // Priority of evaluations rules for normal-reduction
      """g ((\y.y) f)""",
      """g f"""),
    List(
      """(\x.x) (\z.z) ((\y.y) f)""",
      """(\z.z) ((\y.y) f)""",
      """(\y.y) f""",
      """f"""),

    // without substitutions - trickier cases
    List(
      """(\x.x) (\y.y) \z.z""",
      """(\y.y) \z.z""",
      """\z.z"""),
    List(
      """(\x.(\f. f) x) (\y.y) \z.z""",
      """(\x.x) (\y.y) \z.z""",
      """(\y.y) \z.z""",
      """\z.z"""),
    List(
      """(\x.\f.f) (\y.y) \z.z""",
      """(\f.f) \z.z""",
      """\z.z"""),
    List(
      """(\x.\f.x) (\y.y) \z.z""",
      """(\f.\y.y) \z.z""",
      """\y.y"""),
    List(
      """(\x.x \y.y ((\f.g) \z.z)) (\f.f) g""",
      """(\x.x \y.y g) (\f.f) g""",
      """((\f.f) \y.y g) g""",
      """(\y.y g) g""",
      """g g"""), // pam-padam-pa
    List(
      """(\x.x \y.y ((\f.g) \z.z)) ((\f.f) g)""",
      """(\x.x \y.y g) ((\f.f) g)""",
      """(\f.f) g \y.y g""",
      """g \y.y g"""),
    List(
      """(\x.x \y.y ((\f.g) \z.z)) ((\f.f) g) (\x.x x) y""",
      """(\x.x \y.y g) ((\f.f) g) (\x.x x) y""",
      """((\f.f) g \y.y g) (\x.x x) y""",
      """g (\y.y g) (\x.x x) y""" // We are stuck, because y is applied to "g (\y.y g) (\x.x x)"
      // and not only to (\x.x x)
      ),
    List(
      """(\x.x \y.y ((\f.g) \z.z)) ((\f.f) g) ((\x.x) x) y""",
      """(\x.x \y.y g) ((\f.f) g) ((\x.x) x) y""",
      """((\f.f) g \y.y g) ((\x.x) x) y""",
      """g (\y.y g) ((\x.x) x) y""", // This time, we can do one more red. since there is still a redex
      """g (\y.y g) x y"""),
    // TODO: more tests as the one above are welcome, since these kind of cases, were normal-red won't
    // stuck and other reduction strategies will, distinguish them

    List(
      """(\x.x \y.y ((\f.f) \g.g)) ((\f.f) \y.y) (\x.x x) y""",
      """(\x.x \y.y \g.g) ((\f.f) \y.y) (\x.x x) y""",
      """(\f.f) (\y.y) (\y.y \g.g) (\x.x x) y""",
      """(\y.y) (\y.y \g.g) (\x.x x) y""",
      """(\y.y \g.g) (\x.x x) y""",
      """(\x.x x) (\g.g) y""",
      """(\g.g) (\g.g)  y""",
      """(\g.g)  y""",
      """y"""),
    List(
      """(\x.x \y.y ((\f.f) \g.g)) ((\f.f) \y.y) ((\x.x) x) y""",
      """(\x.x \y.y \g.g) ((\f.f) \y.y) ((\x.x) x) y""",
      """(\f.f) (\y.y) (\y.y \g.g) ((\x.x) x) y""",
      """(\y.y) (\y.y \g.g) ((\x.x) x) y""",
      """(\y.y \g.g) ((\x.x) x) y""",
      """((\x.x) x) (\g.g) y""",
      """x (\g.g) y"""),

    List())

  // TODO: Are there interesting cases which do not terminate?

  def normReducer(t: Term): Term = Untyped.reduceNormalOrder(t)
  def assertEq(t: Term, l: List[String]): Unit = l match {
    case Nil => // No more reductions are expected
      assert(t.toRawString === normReducer(t).toRawString, "term should not reduce further")
    case x :: xs => {
      assert(t.toString === x) // the reduction step is OK
      assertEq(normReducer(t), xs) // test the following reductions
    }
  }

  "The normal-reduction strategy" should {
    nrCasesWhichTerminate foreach {
      redSeq =>
        redSeq match {
          case Nil => {} // Discard empty test-cases
          case x :: _ => {
            "correcty reduce " + x in {
              try {
                // We must ensure that the strings in each reduc. sequence List[String]
                // are formatted as they were produced by the parser
                // e.g. "(x)" should become "x"
                // 		"\y. (y x)" should become "\y. y x"
                // 		"\y.y" should become "\y. y"
                val formattedReqSeq = redSeq map { s => Untyped.parseOrDie(s).toString }
                assertEq(Untyped.parseOrDie(x), formattedReqSeq)
              } catch {
                case Untyped.ParseException(e) => fail(e)
              }
            }
          }
        }
    }
  }

}
