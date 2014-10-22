package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  // A trivial printer, for the pretty-printer debugging process
  def toRawString: String
  // The pretty printer, contains some logic, hence not trivial
  def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false): String
  final override def toString = prettyString()
}

case object True extends Term {
  override def toRawString = "true"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "true"
}

case object False extends Term {
  override def toRawString = "false"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "false"
}
case object Zero extends Term {
  override def toRawString = "0"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "0"
}

case class Var(name: String) extends Term {
  override def toRawString = name
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = name
}

case class Abs(x: Var, typ: Type, body: Term) extends Term {
  override def toRawString = "Abs(" + x.toRawString + ":" + typ.toRawString + ", " + body.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = {
    def open = if (par) "(" else ""
    def close = if (par) ")" else ""

    open + "\\" + x + ":" + typ + "." + body + close
  }
}

case class App(t1: Term, t2: Term) extends Term {
  override def toRawString = "App(" + t1.toRawString + ", " + t2.toRawString + ")"
  override def prettyString(par: Boolean, forceRighParInInnerTerm: Boolean) = {
    val (lpar, rpar) = (t1, t2) match {
      case (l: Abs, r: App) => (true, true)
      case (l: Abs, _) => (true, false)
      case (_, r: App) => (false, true)
      case _ => (false, false)
    }

    // Corner case: t = App{ App( ? , Abs), ? } -> the Abs should be protected with par
    val cornerCase = (t1, t2) match {
      case (App(_, lr: Abs), _) => true
      case _ => false
    }

    def open = if (par) "(" else ""
    def close = if (par) ")" else ""

    open + t1.prettyString(lpar, cornerCase) + " " + t2.prettyString(rpar || forceRighParInInnerTerm) + close
  }
}

case class Succ(t: Term) extends Term {
  def numStringWhenPossible(t: Term): String = SimplyTyped.isNumericVal(t) match {
    case true => SimplyTyped.convertToNum(t).toString
    case _ => "Succ(" + t.toRawString + ")"
  }
  override def toRawString = numStringWhenPossible(Succ(t))
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = numStringWhenPossible(Succ(t))
}

case class Pred(t: Term) extends Term {
  override def toRawString = "Pred(" + t.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "Pred(" + t + ")"
}

case class IsZero(t: Term) extends Term {
  override def toRawString = "IsZero(" + t.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "IsZero(" + t + ")"
}

case class If(cond: Term, zen: Term, elze: Term) extends Term {
  override def toRawString = "If(" + cond.toRawString + ", " + zen.toRawString + ", " + elze.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "if " + cond + " then " + zen + " else " + elze
}

case class Pair(fst: Term, snd: Term) extends Term {
  override def toRawString = "Pair(" + fst.toRawString + ", " + snd.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "{" + fst + "," + snd + "}"
}

case class First(p: Term) extends Term {
  override def toRawString = "Fst(" + p.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "fst(" + p + ")"
}

case class Second(p: Term) extends Term {
  override def toRawString = "Snd(" + p.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "snd(" + p + ")"
}

/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object Bool extends Type {
  override def toRawString = "Bool"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "Bool"
}

case object Nat extends Type {
  override def toRawString = "Nat"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "Nat"
}

case class Function(i: Type, o: Type) extends Type {
  override def toRawString = "[" + i.toRawString + "->" + o.toRawString + "]"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = i + "->" + o
}

case class Product(fst: Type, snd: Type) extends Type {
  override def toRawString = "[" + fst.toRawString + "*" + snd.toRawString + "]"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = fst + "*" + snd
}

