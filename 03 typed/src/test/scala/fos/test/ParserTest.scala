package fos.test

import org.scalatest._

class ParserTest extends WordSpec with Matchers {

  import fos.{ SimplyTyped, Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Bool, Nat, Function, Product }
  import fos.test.helpers.Helper._

  val id_b_s = """\x: Bool. x"""
  val id_n_s = """\x: Nat . x"""

  val tests = /* Input -> Term */
    // Simple trees
    """true""" -> True ::
      """false""" -> False ::
      """if true then true else true """ -> If(True, True, True) ::
      """if false then true else true """ -> If(False, True, True) ::
      """if true then false else true """ -> If(True, False, True) ::
      """if true then true else false """ -> If(True, True, False) ::
      """0""" -> Zero ::
      """1""" -> Succ(Zero) ::
      """2""" -> Succ(Succ(Zero)) ::
      """5""" -> Succ(Succ(Succ(Succ(Succ(Zero))))) ::
      """pred 0""" -> Pred(Zero) ::
      """pred 1""" -> Pred(Succ(Zero)) ::
      """succ 0""" -> Succ(Zero) ::
      """succ 1""" -> Succ(Succ(Zero)) ::
      """x""" -> x ::
      """y""" -> y ::
      """xy""" -> Var("xy") ::
      """a$1""" -> Var("a$1") ::
      """alpha123""" -> Var("alpha123") ::
      id_b_s -> id_b ::
      id_n_s -> id_n ::
      """\x: Bool -> Bool. x""" -> Abs(x, Function(Bool, Bool), x) ::
      """\x: Bool * Bool. x""" -> Abs(x, Product(Bool, Bool), x) ::
      """x y""" -> App(x, y) ::
      """(x)""" -> x ::
      """(((x y)))""" -> App(x, y) ::
      id_b_s + " " + id_n_s -> App(id_b, id_n) ::
      // TODO add pairs
      // Complex trees
      "(" + id_b_s + " " + id_n_s + """) \x: Nat. \y: Nat. \z: Nat * Nat -> Nat. z x y""" -> App(App(id_b, id_n), Abs(x, Nat, Abs(y, Nat, Abs(z, Function(Product(Nat, Nat), Nat), App(App(z, x), y))))) ::
      // TODO add more complex trees
      List[(String, Term)]()

  "The parser" should {
    tests foreach {
      case (input, ast) => "procude the correct AST with input " + input in {
        try {
          val res = parseOrDie(input)
          assert(res === ast)
        } catch {
          case e /*SimplyTyped.ParseException(e)*/ => fail(e)
        }
      }
    }
  }

}
