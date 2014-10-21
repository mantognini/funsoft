package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def toRawString: String
  def prettyString( /* TODO ARG */ ): String
  final override def toString = prettyString
}

case object True extends Term {
  override def toRawString = "true"
  override def prettyString = "true"
}

case object False extends Term {
  override def toRawString = "false"
  override def prettyString = "false"
}
case object Zero extends Term {
  override def toRawString = "0"
  override def prettyString = "0"
}

case class Var(name: String) extends Term {
  override def toRawString = name
  override def prettyString = name
}

case class Abs(x: Var, typ: Type, body: Term) extends Term {
  override def toRawString = "Abs(" + x.toRawString + ":" + typ.toRawString + ", " + body.toRawString + ")"
  override def prettyString = ???
}

case class App(t1: Term, t2: Term) extends Term {
  override def toRawString = "App(" + t1.toRawString + ", " + t2.toRawString + ")"
  override def prettyString = ???
}

case class Succ(t: Term) extends Term {
  override def toRawString = SimplyTyped.isNumericVal(t) match {
    case true => SimplyTyped.convertToNum(t).toString
    case _ => "Succ(" + t.toRawString + ")"
  }
  override def prettyString = ???
}

case class Pred(t: Term) extends Term {
  override def toRawString = "Pred(" + t.toRawString + ")"
  override def prettyString = ???
}

case class IsZero(t: Term) extends Term {
  override def toRawString = "IsZero(" + t.toRawString + ")"
  override def prettyString = ???
}

case class If(cond: Term, zen: Term, elze: Term) extends Term {
  override def toRawString = "If(" + cond.toRawString + ", " + zen.toRawString + ", " + elze.toRawString + ")"
  override def prettyString = ???
}

case class Pair(fst: Term, snd: Term) extends Term {
  override def toRawString = "Pair(" + fst.toRawString + ", " + snd.toRawString + ")"
  override def prettyString = ???
}

case class First(p: Term) extends Term {
  override def toRawString = "Fst(" + p.toRawString + ")"
  override def prettyString = ???
}

case class Second(p: Term) extends Term {
  override def toRawString = "Snd(" + p.toRawString + ")"
  override def prettyString = ???
}

/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object Bool extends Type {
  override def toRawString = "Bool"
  override def prettyString = "Bool"
}

case object Nat extends Type {
  override def toRawString = "Nat"
  override def prettyString = "Nat"
}

case class Function(i: Type, o: Type) extends Type {
  override def toRawString = "[" + i.toRawString + " -> " + o.toRawString + "]"
  override def prettyString = ???
}

case class Product(fst: Type, snd: Type) extends Type {
  override def toRawString = "[" + fst.toRawString + " x " + snd.toRawString + "]"
  override def prettyString = ???
}

