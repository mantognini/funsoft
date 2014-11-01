package fos.test

import org.scalatest._

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.{ Term, True, False, Zero, If, Succ, Pred, IsZero, Var, Abs, App, Pair, First, Second, Inl, Inr, Case }
  import fos.{ Type, Bool, Nat, Function, Product, Sum }
  import fos.test.helpers.ttools
  import fos.test.SumTypesTest

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

  def toRawString(t: Term): String = t match {
    case Product(fst, snd) => "[" + toRawString(fst) + "*" + toRawString(snd) + "]"
    case Function(i, o) => "[" + toRawString(i) + "->" + toRawString(o) + "]"
    case Nat() => "Nat"
    case Bool() => "Bool"
    case Second(p) => "snd(" + toRawString(p) + ")"
    case First(p) => "fst(" + toRawString(p) + ")"
    case Pair(fst, snd) => "pair(" + toRawString(fst) + ", " + toRawString(snd) + ")"
    case If(cond, zen, elze) => "if(" + toRawString(cond) + ", " + toRawString(zen) + ", " + toRawString(elze) + ")"
    case IsZero(t) => "iszero " + toRawString(t)
    case Pred(t) => "pred " + toRawString(t)
    case Succ(t) => "succ(" + toRawString(t) + ")"
    case App(t1, t2) => "App(" + toRawString(t1) + ", " + toRawString(t2) + ")"
    case Abs(x, typ, body) => "Abs(" + toRawString(x) + ":" + toRawString(typ) + ", " + toRawString(body) + ")"
    case Var(name) => name
    case Zero() => "0"
    case False() => "false"
    case True() => "true"

    case Sum(typ1, typ2) => "[" + toRawString(typ1) + "+" + toRawString(typ2) + "]"
    case Inl(t, typ) => "(inl " + toRawString(t) + " as " + toRawString(typ) + ")"
    case Inr(t, typ) => "(inr " + toRawString(t) + " as " + toRawString(typ) + ")"
    case Case(t, inlVar, inlTerm, inrVar, inrTerm) =>
      "(case " + toRawString(t) + " of inl " + toRawString(inlVar) + "=>" + toRawString(inlTerm) + " | inr " +
        toRawString(inrVar) + "=>" + toRawString(inrTerm) + ")"

    case _ => throw new NotImplementedError()
  }
}