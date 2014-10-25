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
    Product(Bool, Nat) -> "Bool*Nat")

  // TODO: Test right-assoc' of Product and Function
  // TODO: Implement these type tests
}