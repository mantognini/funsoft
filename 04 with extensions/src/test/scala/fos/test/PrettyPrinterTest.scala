package fos.test

import org.scalatest._

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.{ Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Pair, First, Second, Inl, Inr, Case }
  import fos.{ Type, Bool, Nat, Function, Product, Sum }
  import fos.test.helpers.ttools
  import fos.test.SumTypesTest

  def toRawString(t: Term) = ttools.toRawString(t)

  behavior of "The pretty printer"

  val termsToString = ttools.canonicalCases map { _.swap }

  termsToString.foreach {
    case (ast, expr) => it should "properly print " + expr + ". NB, AST is " + toRawString(ast) in {
      ast.toString shouldEqual expr
    }
  }

  ttools.typeCanonicalCases.foreach {
    case (ast, expr) => it should "properly print " + expr + ". NB, AST is " + toRawString(ast) in {
      ast.toString shouldEqual expr
    }
  }

  SumTypesTest.toStringCases.foreach {
    case (ast, expr) => it should "properly print " + expr + ". NB, AST is " + toRawString(ast) in {
      ast.toString shouldEqual expr
    }
  }
}