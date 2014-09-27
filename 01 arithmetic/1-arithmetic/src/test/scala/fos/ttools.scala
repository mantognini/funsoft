package fos

import org.scalatest.Exceptional
import scala.collection.immutable.List
import scala.collection.immutable.Map

object ttools {

  def testReduction(redFun: Term => (String => Unit) => Unit, testFun: String => String => Unit)(t: Term, expected: String): Unit = {
    redFun(t)(testFun(expected))
  }

  def getSomeEvaluationTestingValues(): Map[String, List[(Term, String)]] = {
    Map(
      "values" -> List(
        (False, "False"),
        (True, "True"),
        (Zero, "Zero"),
        (Succ(Zero), "Succ(Zero)"),
        (Succ(Pred(Zero)), "Succ(Zero)"),
        (Succ(Succ(Zero)), "Succ(Succ(Zero))")),
      "if" -> List(
        (If(True, True, False), "True"),
        (If(False, True, False), "False"),
        (If(If(True, True, False), True, False), "True"),
        (If(If(False, True, False), True, False), "False"),
        (If(If(True, True, False), If(False, True, False), False), "False")),
      "pred" -> List(
        (Pred(Zero), "Zero"),
        (Pred(Succ(Zero)), "Zero")),
      "isZero" -> List(
        (IsZero(Zero), "True"),
        (IsZero(Succ(Zero)), "False"),
        (IsZero(Succ(Pred(Zero))), "False")),
      "smallStepCongruences" -> List(
        (If(IsZero(Zero), True, False), "True"),
        (If(IsZero(Pred(Succ(Zero))), True, False), "True"),
        (If(IsZero(Succ(Pred(Zero))), True, False), "False"),
        (IsZero(Pred(Zero)), "True"),
        (IsZero(Pred(Succ(Zero))), "True"),
        (IsZero(Pred(Succ(Succ(Zero)))), "False"),
        (Pred(Pred(Succ(Succ(Succ(Zero))))), "Succ(Zero)"),
        (Succ(Pred(Pred(Succ(Zero)))), "Succ(Zero)"),
        (IsZero(Pred(If(True, Succ(Zero), False))), "True")),
      "complex" -> List(
        (If(IsZero(Pred(Succ(Zero))), Succ(Pred(Zero)), Pred(Zero)), "Succ(Zero)"),
        (IsZero(If(If(IsZero(Succ(Zero)), True, False), True, Succ(Zero))), "False")))
  }

}