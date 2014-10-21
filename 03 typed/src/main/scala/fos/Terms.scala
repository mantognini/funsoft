package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def prettyString( /* TODO ARG */ ): String
  final override def toString = prettyString
}

case object True extends Term {
  override def prettyString = "true"
}

case object False extends Term {
  override def prettyString = "false"
}
case object Zero extends Term {
  override def prettyString = "0"
}

case class Var(name: String) extends Term {
  override def prettyString = ???
}

case class Abs(x: Var, typ: Type, body: Term) extends Term {
  override def prettyString = ???
}

case class App(t1: Term, t2: Term) extends Term {
  override def prettyString = ???
}

case class Succ(t: Term) extends Term {
  override def prettyString = ???
}

case class Pred(t: Term) extends Term {
  override def prettyString = ???
}

case class IsZero(t: Term) extends Term {
  override def prettyString = ???
}

case class If(cond: Term, zen: Term, elze: Term) extends Term {
  override def prettyString = ???
}

case class Pair(fst: Term, snd: Term) extends Term {
  override def prettyString = ???
}

case class First(p: Term) extends Term {
  override def prettyString = ???
}

case class Second(p: Term) extends Term {
  override def prettyString = ???
}

/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object Bool extends Type {
  override def prettyString = "Bool"
}

case object Nat extends Type {
  override def prettyString = "Nat"
}

case class Function(i: Type, o: Type) extends Type {
  override def prettyString = ???
}

case class Product(fst: Type, snd: Type) extends Type {
  override def prettyString = ???
}

