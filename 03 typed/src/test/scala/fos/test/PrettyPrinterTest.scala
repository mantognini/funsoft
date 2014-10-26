package fos.test

import org.scalatest._
import fos.{ Type, Bool, Nat, Function, Product }

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.test.helpers.ttools

  behavior of "The pretty printer"

  val termsToString = ttools.canonicalCases map { _.swap }

  termsToString.foreach {
    case (ast, expr) => it should "properly print " + expr + ". NB, AST is " + ast.toRawString in {
      ast.toString shouldEqual expr
    }
  }

  val typeExpressions = Map[Type, String](
    Bool -> "Bool",
    Nat -> "Nat",
    Function(Nat, Nat) -> "Nat->Nat",
    Product(Bool, Nat) -> "Bool*Nat",

    // Function is right associative
    Function(Nat, Function(Nat, Nat)) -> "Nat->Nat->Nat",
    Function(Function(Nat, Nat), Nat) -> "(Nat->Nat)->Nat",

    Function(Nat, Function(Bool, Function(Nat, Bool))) -> "Nat->Bool->Nat->Bool",
    Function(Function(Function(Nat, Bool), Nat), Bool) -> "((Nat->Bool)->Nat)->Bool",

    Function(Nat, Function(Function(Bool, Nat), Bool)) -> "Nat->(Bool->Nat)->Bool",
    Function(Function(Nat, Function(Bool, Nat)), Bool) -> "(Nat->Bool->Nat)->Bool",
    Function(Function(Nat, Bool), Function(Nat, Bool)) -> "(Nat->Bool)->Nat->Bool",

    // Product is right associative
    Product(Nat, Product(Nat, Nat)) -> "Nat*Nat*Nat",
    Product(Product(Nat, Nat), Nat) -> "(Nat*Nat)*Nat",

    Product(Nat, Function(Bool, Function(Nat, Bool))) -> "Nat*Bool*Nat*Bool",
    Product(Product(Product(Nat, Bool), Nat), Bool) -> "((Nat*Bool)*Nat)*Bool",

    Product(Nat, Product(Product(Bool, Nat), Bool)) -> "Nat*(Bool*Nat)*Bool",
    Product(Product(Nat, Product(Bool, Nat)), Bool) -> "(Nat*Bool*Nat)*Bool",
    Product(Product(Nat, Bool), Product(Nat, Bool)) -> "(Nat*Bool)*Nat*Bool" // Product takes precedence over Function
    // TODO: Write some examples
    )

  // TODO: Implement these type tests
}