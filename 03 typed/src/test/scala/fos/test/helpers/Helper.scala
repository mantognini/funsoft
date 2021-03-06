package fos.test.helpers

object Helper {

  import fos.{ SimplyTyped, Term }
  import SimplyTyped.{ Success, Failure, Error }
  import org.scalatest.Assertions

  case class ParseException(e: String) extends Exception
  def error(msg: String, next: SimplyTyped.Input): Nothing = { throw new ParseException(msg + "\n" + next.pos.longString) }

  implicit val termParser: String => SimplyTyped.ParseResult[Term] = SimplyTyped.parse
  def parseOrDie(input: String)(implicit parser: String => SimplyTyped.ParseResult[Term]): Term = parser(input) match {
    case Success(ast, _) => ast
    case Failure(msg, next) => error(msg, next)
    case Error(msg, next) => error(msg, next)
  }

  def tryComputeOrFail[E, R](fun: => R)(implicit manifest: Manifest[E]): R =
    try {
      fun
    } catch {
      case e: E => Assertions.fail(e)
    }

  def tryOrFail[E](fun: => Unit)(implicit manifest: Manifest[E]): Unit =
    tryComputeOrFail[E, Unit](fun)

  def parseOrFail(input: String)(implicit parser: String => SimplyTyped.ParseResult[Term]): Term =
    tryComputeOrFail[ParseException, Term] { parseOrDie(input)(parser) }

  // Define a few vars
  import fos.{ Var }
  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")

  val t = Var("t")

  val f = Var("f")
  val g = Var("g")

  // Define few numbers
  import fos.{ Succ, Zero }
  val one = Succ(Zero())
  val two = Succ(Succ(Zero()))
  val three = Succ(Succ(Succ(Zero())))

  // Define a few functions
  import fos.{ Abs, Bool, Nat }
  val id_b = Abs(x, Bool(), x)
  val id_n = Abs(x, Nat(), x)

  // Define a few pairs
  import fos.{ Pair }
  val p_ab = Pair(a, b)
  val p_xy = Pair(x, y)
  val p_id_bn = Pair(id_b, id_n)
}
