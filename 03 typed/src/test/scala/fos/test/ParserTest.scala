package fos.test

import org.scalatest._

class ParserTest extends WordSpec with Matchers {

  import fos.{ SimplyTyped, Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Pair, First, Second, Bool, Nat, Function, Product }
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
      // Test Zero
      """iszero 0""" -> IsZero(Zero) ::
      """iszero x""" -> IsZero(x) ::
      """iszero 1""" -> IsZero(Succ(Zero)) ::
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
      // Let
      """let x: Nat = 0 in x""" -> App(Abs(x, Nat, x), Zero) ::
      """let id: Nat -> Nat = \x: Nat. x in id 0""" -> App(Abs(Var("id"), Function(Nat, Nat), App(Var("id"), Zero)), Abs(x, Nat, x)) ::
      """let x: Nat = 0 in let y: Nat = 1 in x y""" -> App(Abs(x, Nat, App(Abs(y, Nat, App(x, y)), Succ(Zero))), Zero) ::
      """let z: Nat = 0 in let id: Nat -> Nat = \x: Nat. x in id z""" -> App(Abs(z, Nat, App(Abs(Var("id"), Function(Nat, Nat), App(Var("id"), z)), Abs(x, Nat, x))), Zero) ::
      // Pairs
      """{ x, y }""" -> p_xy ::
      "{ " + id_b_s + ", " + id_n_s + " }" -> p_id_bn ::
      """{ { a, b }, { x, y } }""" -> Pair(p_ab, p_xy) ::
      """fst { x, y }""" -> First(p_xy) ::
      """snd { x, y }""" -> Second(p_xy) ::
      """fst fst { { a, b }, { x, y } }""" -> First(First(Pair(p_ab, p_xy))) ::
      """fst snd { { a, b }, { x, y } }""" -> First(Second(Pair(p_ab, p_xy))) ::
      """snd fst { { a, b }, { x, y } }""" -> Second(First(Pair(p_ab, p_xy))) ::
      """snd snd { { a, b }, { x, y } }""" -> Second(Second(Pair(p_ab, p_xy))) ::
      // Complex trees
      "(" + id_b_s + " " + id_n_s + """) \x: Nat. \y: Nat. \z: Nat * Nat -> Nat. z x y""" -> App(App(id_b, id_n), Abs(x, Nat, Abs(y, Nat, Abs(z, Function(Product(Nat, Nat), Nat), App(App(z, x), y))))) ::
      // TODO add more complex trees
      List[(String, Term)]()

  val typeTests = /* Input -> Type */
    """Nat""" -> Nat ::
      Nil

  def processTests(msgPrefix: String, tests: List[(String, Term)], parser: String => Term) {
    tests foreach {
      case (input, ast) => msgPrefix + input in {
        try {
          val res = parser(input)
          assert(res === ast)
        } catch {
          case ParseException(e) => fail(e)
        }
      }
    }
  }

  def typeParser(input: String): SimplyTyped.ParseResult[Term] = SimplyTyped.phrase(SimplyTyped.Type)(new SimplyTyped.lexical.Scanner(input))

  "The parser" should {
    processTests("procude the correct AST with input ", tests, input => parseOrDie(input))
    processTests("procude the correst AST for Types with input ", typeTests, input => parseOrDie(input)(typeParser))
  }

}
