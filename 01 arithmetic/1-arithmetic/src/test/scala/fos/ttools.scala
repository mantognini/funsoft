package fos

import org.scalatest.Exceptional
import scala.collection.immutable.List
import scala.collection.immutable.Map

object ttools {

  def testReduction(redFun: Term => (String => Unit) => Unit, testFun: String => String => Unit)(t: Term, expected: String): Unit = {
    redFun(t)(testFun(expected))
  }

  def getSomeEvaluationTestingValues(): Map[String, List[Tuple2[Term, String]]] = {
    Map(
      "values" -> List(
        (False, "False"),
        (True, "True"),
        (Succ(Zero), "Succ(Zero)"),
        (Succ(Succ(Zero)), "Succ(Succ(Zero))")))
  }

}