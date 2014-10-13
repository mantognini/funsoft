package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  // Print AST with all ()'s and case class names except Var
  // e.g. Abs(Var(x), App(Var(x), Var(y))) --[ toRawString ]--> Abs(x, App(x, y))
  def toRawString: String

  // Pretty printing:
  def prettyString(par: Boolean, forceRighParInInnerTerm: Boolean = false): String // abstract
  final override def toString = prettyString(false) // no parentheses by default
}

case class Var(name: String) extends Term // x
{
  override def toRawString = name

  override def prettyString(par: Boolean, forceRighParInInnerTerm: Boolean) = name // ignore par for variable
}

case class Abs(arg: Var, body: Term) extends Term // \x.t
{
  override def toRawString = "Abs(" + arg.toRawString + ", " + body.toRawString + ")"

  override def prettyString(par: Boolean, forceRighParInInnerTerm: Boolean) = {
    def open = if (par) "(" else ""
    def close = if (par) ")" else ""

    open + "\\" + arg + ". " + body + close
  }
}

case class App(left: Term, right: Term) extends Term // t t
{
  override def toRawString = "App(" + left.toRawString + ", " + right.toRawString + ")"

  override def prettyString(par: Boolean, forceRighParInInnerTerm: Boolean) = {
    val (lpar, rpar) = (left, right) match {
      case (l: Abs, r: App) => (true, true)
      case (l: Abs, _) => (true, false)
      case (_, r: App) => (false, true)
      case _ => (false, false)
    }

    // Corner case: t = App{ App( ? , Abs), ? } -> the Abs should be protected with par
    val cornerCase = (left, right) match {
      case (App(_, lr: Abs), _) => true
      case _ => false
    }

    def open = if (par) "(" else ""
    def close = if (par) ")" else ""

    open + left.prettyString(lpar, cornerCase) + " " + right.prettyString(rpar || forceRighParInInnerTerm) + close
  }
}