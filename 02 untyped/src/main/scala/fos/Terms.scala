package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class Var(name: String) extends Term // x
{ override def toString = name }

case class Abs(arg: Var, body: Term) extends Term // \x.t
{ override def toString = "\\" + arg + ". " + body }

case class App(left: Term, right: Term) extends Term // t t
{
  override def toString = left match {
    case App(_, _) => "(" + left + ") " + right
    case _ => left + " " + right
  }
}