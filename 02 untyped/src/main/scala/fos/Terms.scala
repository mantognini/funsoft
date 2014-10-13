package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def toRawString: String // TODO add doc. why is it needed?

  // Pretty printing:
  def prettyString(par: Boolean): String // abstract
  final override def toString = prettyString(false) // no parentheses by default
}

case class Var(name: String) extends Term // x
{
  override def toRawString = name

  override def prettyString(par: Boolean) = name // ignore par for variable
}

case class Abs(arg: Var, body: Term) extends Term // \x.t
{
  override def toRawString = "Abs(" + arg.toRawString + ", " + body.toRawString + ")"

  override def prettyString(par: Boolean) = {
    def open = if (par) "(" else ""
    def close = if (par) ")" else ""

    open + "\\" + arg + ". " + body + close
  }
}

case class App(left: Term, right: Term) extends Term // t t
{
  override def toRawString = "App(" + left.toRawString + ", " + right.toRawString + ")"

  override def prettyString(par: Boolean) = {
    val (lpar, rpar) = (left, right) match {
      case (l: Abs, r: Var) => (true, false)
      case (l: Abs, r: Abs) => (true, false)
      case (l: Abs, r: App) => (true, true)
      case (l: App, r: Var) => (false, false)
      case (l: App, r: Abs) => (false, false)
      case (l: App, r: App) => (false, true)
      case (l: Var, r: Var) => (false, false)
      case (l: Var, r: Abs) => (false, false)
      case (l: Var, r: App) => (false, true)
    }

    def open = if (par) "(" else ""
    def close = if (par) ")" else ""

    open + left.prettyString(lpar) + " " + right.prettyString(rpar) + close
  }
}