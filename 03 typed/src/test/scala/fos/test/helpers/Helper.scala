package fos.test.helpers

object Helper {

  import fos.{ SimplyTyped, Term }
  import SimplyTyped.{ Success, Failure, Error }

  case class ParseException(e: String) extends Exception
  def error(msg: String, next: SimplyTyped.Input): Nothing = { throw new ParseException(msg + "\n" + next.pos.longString) }

  implicit val termParser: String => SimplyTyped.ParseResult[Term] = SimplyTyped.parse
  def parseOrDie(input: String)(implicit parser: String => SimplyTyped.ParseResult[Term]): Term = parser(input) match {
    case Success(ast, _) => ast
    case Failure(msg, next) => error(msg, next)
    case Error(msg, next) => error(msg, next)
  }

  // Define a few vars
  import fos.{ Var }
  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")

  val f = Var("f")
  val g = Var("g")

  // Define a few functions
  import fos.{ Abs, Bool, Nat }
  val id_b = Abs(x, Bool, x)
  val id_n = Abs(x, Nat, x)

  // Define a few pairs
  import fos.{ Pair }
  val p_ab = Pair(a, b)
  val p_xy = Pair(x, y)
  val p_id_bn = Pair(id_b, id_n)
}
