package fos.test

import org.scalatest._

class ParserTest extends WordSpec with Matchers {

  import fos.{ SimplyTyped, Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Bool, Nat, Function, Product }
  import fos.test.helpers.Helper._

  val id_b_s = """\x: Bool. x"""
  val id_n_s = """\x: Nat . x"""

  val tests = /* Input -> Term */
    // Boolean
    """true""" -> True ::
      """false""" -> False ::
      // If statements
      """if true then true else true """ -> If(True, True, True) ::
      """if false then true else true """ -> If(False, True, True) ::
      """if true then false else true """ -> If(True, False, True) ::
      """if true then true else false """ -> If(True, True, False) ::
      // Numbers
      """0""" -> Zero ::
      """1""" -> Succ(Zero) ::
      """2""" -> Succ(Succ(Zero)) ::
      """5""" -> Succ(Succ(Succ(Succ(Succ(Zero))))) ::
      """pred 0""" -> Pred(Zero) ::
      """pred 1""" -> Pred(Succ(Zero)) ::
      """succ 0""" -> Succ(Zero) ::
      """succ 1""" -> Succ(Succ(Zero)) ::
      // Variables
      """x""" -> x ::
      """y""" -> y ::
      """xy""" -> Var("xy") ::
      // """a$1""" -> Var("a$1") :: // This is in fact rejected by StandardTokenParsers.ident
      """alpha123""" -> Var("alpha123") ::
      // Identity Functions
      id_b_s -> id_b ::
      id_n_s -> id_n ::
      // Function types
      """\x: Bool -> Nat. x""" -> Abs(x, Function(Bool, Nat), x) ::
      """\x: Bool -> Nat -> Bool. x""" -> Abs(x, Function(Bool, Function(Nat, Bool)), x) ::
      """\x: (Bool -> Nat) -> Bool. x""" -> Abs(x, Function(Function(Bool, Nat), Bool), x) ::
      """\x: (Bool -> Nat) -> (Nat -> Bool). x""" -> Abs(x, Function(Function(Bool, Nat), Function(Nat, Bool)), x) ::
      // Product types
      """\x: Bool * Nat. x""" -> Abs(x, Product(Bool, Nat), x) ::
      """\x: Bool * Bool * Nat. x""" -> Abs(x, Product(Bool, Product(Bool, Nat)), x) ::
      """\x: (Bool * Nat) * Bool. x""" -> Abs(x, Product(Product(Bool, Nat), Bool), x) ::
      """\x: (Bool * Nat) * (Nat * Bool). x""" -> Abs(x, Product(Product(Bool, Nat), Product(Nat, Bool)), x) ::
      // Mix Function and Product with correct precedence
      """\x: Nat * Nat -> Bool. x""" -> Abs(x, Function(Product(Nat, Nat), Bool), x) ::
      """\x: Nat * Nat -> Bool * Bool. x""" -> Abs(x, Function(Product(Nat, Nat), Product(Bool, Bool)), x) ::
      """\x: Bool -> Nat * Nat -> Bool * Bool. x""" -> Abs(x, Function(Bool, Function(Product(Nat, Nat), Product(Bool, Bool))), x) ::
      // Applications
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
          case ParseException(e) => fail(e)
        }
      }
    }
  }

}
