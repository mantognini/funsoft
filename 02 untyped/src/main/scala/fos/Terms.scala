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
  override def toString = {
    def withPar(t: Term) = t match {
      case Var(_) => t // ignore parentheses for simple var
      case _ => "(" + t + ")"
    }

    def withLeftPar = withPar(left) + " " + right
    def withRightPar = left + " " + withPar(right)
    def withLeftRightPar = withPar(left) + " " + withPar(right)
    def withoutPar = left + " " + right

    (left, right) match {
      case (Abs(_, _), Abs(_, _)) => withLeftRightPar
      case (Abs(_, _), _) => withLeftPar
      case (_, Abs(_, _)) => withRightPar
      case (App(Abs(_, _), _), _) => withoutPar
      case (App(_, _), _) => withLeftPar
      case _ => withoutPar
    }
  }
}