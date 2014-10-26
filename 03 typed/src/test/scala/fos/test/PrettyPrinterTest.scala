package fos.test

import org.scalatest._

class PrettyPrintTest extends FlatSpec with Matchers {

  import fos.{ SimplyTyped, Term, True, False, Zero, If, Pair, First, Second, Succ, Pred, IsZero, Var, Abs, App, Bool, Nat, Function, Product }
  import fos.test.helpers.ttools

  behavior of "The pretty printer"

  val termsToString = ttools.canonicalCases map { _.swap }

  termsToString.foreach {
    case (ast, expr) => it should "properly print " + expr + ". NB, AST is " + toRawString(ast) in {
      ast.toString shouldEqual expr
    }
  }

  // TODO: Implement these type tests using cases in ttools.typeCanonicalCases
  // TODO: Implement Helper for toRawString
  def toRawString(t: Term): String = t match {
    case Product(fst, snd) => "[" + toRawString(fst) + "*" + toRawString(snd) + "]"
    case Function(i, o) => "[" + toRawString(i) + "->" + toRawString(o) + "]"
    case Nat => "Nat"
    case Bool => "Bool"
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
    case Zero => "0"
    case False => "false"
    case True => "true"
  }
}