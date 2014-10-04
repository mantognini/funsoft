package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class Var(name: String) extends Term // x 
case class Abs(arg: Var, body: Term) extends Term // \x.t
case class App(left: Term, right: Term) extends Term // t t
case class Par(t: Term) extends Term // ( t )
