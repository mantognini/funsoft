package fos.test.helpers

object Helper {

  import fos.{ SimplyTyped, Term }
  import SimplyTyped.{ Success, Failure, Error }

  case class ParseException(e: String) extends Exception

  def parseOrDie(input: String) = SimplyTyped.parse(input) match {
    case Success(ast, _) => ast
    case Failure(e, _) => throw new ParseException(e)
    case Error(e, _) => throw new ParseException(e)
  }

  // Define a few vars
  import fos.{ Var }
  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")

  // Define a few functions
  import fos.{ Abs, Bool, Nat }
  val id_b = Abs(x, Bool, x)
  val id_n = Abs(x, Nat, x)
}
