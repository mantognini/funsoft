package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def toRawString = "Term"
}

case class Var(name: String) extends Term // x
{
  override def toString = name
  override def toRawString = "Var(" + name + ")"
}

case class Abs(arg: Var, body: Term) extends Term // \x.t
{
  var mustHaveParenthesis = false
  override def toString = mustHaveParenthesis match {
    case true => "(\\" + arg + ". " + body + ")"
    case _ => "\\" + arg + ". " + body
  }
  override def toRawString = "Abs(" + arg.toRawString + ", " + body.toRawString + ")"
}

case class App(left: Term, right: Term) extends Term // t t
{
  override def toRawString = "App(" + left.toRawString + ", " + right.toRawString + ")"
  override def toString = {
    def withPar(t: Term) = t match {
      case Var(_) => t // ignore parentheses for simple var
      case _ => "(" + t + ")"
    }

    def withLeftPar = withPar(left) + " " + right
    def withRightPar = left + " " + withPar(right)
    def withLeftRightPar = withPar(left) + " " + withPar(right)
    def withoutPar = left + " " + right

    def getRightMostTermIfItIsAnAbs(t: Term): Option[Abs] = t match {
      case Var(_) => None
      case absTerm @ Abs(_, _) => Some(absTerm)
      case App(left, right) => getRightMostTermIfItIsAnAbs(right)
    }

    def addParIfRightMostTermIsAnAbs(t: Term) = getRightMostTermIfItIsAnAbs(t) match {
      case Some(lambdaRightMostTerm) => lambdaRightMostTerm.mustHaveParenthesis = true
      case _ => {}
    }

    (left, right) match {
      // (\x.x) ____
      case (Abs(_, _), App(_, _)) => withLeftRightPar
      case (Abs(_, _), _) => withLeftPar

      // x ____
      case (Var(_), App(_, _)) => withRightPar
      case (Var(_), _) => withoutPar

      // x y ____
      case (App(_, _), App(_, _)) => {
        addParIfRightMostTermIsAnAbs(left)
        withRightPar
      }
      case (_, _) => {
        addParIfRightMostTermIsAnAbs(left)
        withoutPar
      }
    }
  }
}