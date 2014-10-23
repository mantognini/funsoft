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

  // examples of right-most syntax components:
  // if t then t		-->	t		-->	true
  // fst t				--> t		-->	true
  // fst(t)				--> ")"		-->	false
  // ..
  def isRightMostSyntaxComponentATerm(t: Term): Boolean = t match {
    case If(_, _, t) => true
    case Pred(t) => true
    case Succ(t) => true
    case IsZero(t) => true
    case Abs(_, _, t) => true
    case App(_, t) => true
    case First(t) => true
    case Second(t) => true
    case _ => false
  }
  override def prettyString(par: Boolean, forceRighParInInnerTerm: Boolean) = {
    // All rules stated before the APP_RULE (in the grammar) and terminating with
    // a term, may "eat" the following terms. Example (Pred terminates with a term):
    // App(Pred(x), s)	-->	(pred x) s
    // .. but
    // pred x s			-->	Pred(App(x,s)) .. pred is "eating" s
    //
    // Also, APP_RULE is left-associative, we must be careful to put the ()'s
    // when needed. Example:
    // a b c			--> App(App(a,b),c) .. don't put ()'s when not needed
    // .. but
    // a (b c)			--> App(a, App(b,c)) .. put when needed
    val (lpar, rpar) = (t1, t2) match {
      case (l, r: App) if isRightMostSyntaxComponentATerm(l) => (true, true)
      case (l, _) if isRightMostSyntaxComponentATerm(l) => (true, false)
      case (_, r: App) => (false, true)
      case _ => (false, false)
    }

    // If isRightMostSyntaxComponentATerm(t) then t is a << FT >>
    // Corner case: t = App{ App( ? , FT), ? } -> the FT should be protected with ()'s
    // .. or it will "eat" the next terms..
    val cornerCase = (t1, t2) match {
      case (App(_, lr), _) if isRightMostSyntaxComponentATerm(lr) => true
      case _ => false
    }

    def open = if (par) "(" else ""
    def close = if (par) ")" else ""

    open + t1.prettyString(lpar, cornerCase) + " " + t2.prettyString(rpar || forceRighParInInnerTerm) + close
  }
}

case class Succ(t: Term) extends Term {
  override def toRawString = "succ(" + t.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = {
    if (SimplyTyped.isNumericVal(this)) {
      SimplyTyped.convertToNum(this).toString
    } else {
      "succ " + t
    }
  }
}

case class Pred(t: Term) extends Term {
  override def toRawString = "pred " + t.toRawString
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "pred " + t
}

case class IsZero(t: Term) extends Term {
  override def toRawString = "iszero " + t.toRawString
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "iszero " + t
}

case class If(cond: Term, zen: Term, elze: Term) extends Term {
  override def toRawString = "if(" + cond.toRawString + ", " + zen.toRawString + ", " + elze.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "if " + cond + " then " + zen + " else " + elze
}

case class Pair(fst: Term, snd: Term) extends Term {
  override def toRawString = "pair(" + fst.toRawString + ", " + snd.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "{" + fst + "," + snd + "}"
}

case class First(p: Term) extends Term {
  override def toRawString = "fst(" + p.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "fst(" + p + ")"
}

case class Second(p: Term) extends Term {
  override def toRawString = "snd(" + p.toRawString + ")"
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
  // TODO, if (A -> B) -> C, put parenthesis!
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = i + "->" + o
}

case class Product(fst: Type, snd: Type) extends Type {
  override def toRawString = "[" + fst.toRawString + "*" + snd.toRawString + "]"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = fst + "*" + snd
}

