package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  // A trivial printer, for the pretty-printer debugging process
  def toRawString: String
  // The pretty printer, contains some logic, hence not trivial
  def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false): String
  final override def toString = prettyString()

  // TODO: make par implicit parameter??
  final def surroundWithPar(par: Boolean, msg: String): String = {
    def open = if (par) "(" else ""
    def close = if (par) ")" else ""
    open + msg + close
  }
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
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, "\\" + x + ":" + typ + "." + body)
}

case class App(t1: Term, t2: Term) extends Term {
  override def toRawString = "App(" + t1.toRawString + ", " + t2.toRawString + ")"

  // Examples where we test if the right-most syntax components is a term:
  // if t then t		-->	t		-->	true
  // fst t				--> t		-->	true
  // fst(t)				--> ")"		-->	false
  // ..
  // If term t has a term in its right-most syntax component
  // 		example: fst t, Abs t, pred t,
  //		but not: numericLit, "false", "true", x (the variable)
  // and is, in the grammar definition (Fig.1), stated before the APP_RULE,
  // then we call it a << FT >> "Finish with a Term (and is before APP_RULE)"
  //.. why do we care?
  // 
  // Because all rules stated before the APP_RULE (in the grammar) and terminating with
  // a term, may "eat" the following terms. Example ("pred t" terminates with a term):
  // App(Pred(x), s)	-->	(pred x) s
  // .. but
  // pred x s			-->	Pred(App(x,s)) .. pred is "eating" s
  // ..
  // .. note that if "pred t" would have been after APP_RULE, then APP_RULE
  // would have been applied before it .. and we would have had
  // pred x s			--> App(Pred(x), s)
  //
  // Hence, we should be careful with FT terms and put parenthis around them
  // whenever needed
  def isFT(t: Term): Boolean = t match {
    case If(_, _, t) => true
    case Pred(t) => true
    case Succ(t) => true
    case IsZero(t) => true
    case Abs(_, _, t) => true
    case First(t) => true
    case Second(t) => true
    case _ => false
  }

  override def prettyString(par: Boolean, forceRighParInInnerTerm: Boolean) = {
    // APP_RULE is left-associative, we must be careful to put the ()'s
    // when needed. Example:
    // a b c			--> App(App(a,b),c) .. don't put ()'s when not needed
    // .. but
    // a (b c)			--> App(a, App(b,c)) .. put when needed
    val (lpar, rpar) = (t1, t2) match {
      case (l, r: App) if isFT(l) => (true, true)
      case (l, _) if isFT(l) => (true, false)
      case (_, r: App) => (false, true)
      case _ => (false, false)
    }

    // Corner case: t = App{ App( ? , FT), ? } -> the FT should be protected with ()'s
    // .. or it will "eat" the next terms..
    val cornerCase = (t1, t2) match {
      case (App(_, lr), _) if isFT(lr) => true
      case _ => false
    }

    surroundWithPar(par, t1.prettyString(lpar, cornerCase) + " " + t2.prettyString(rpar || forceRighParInInnerTerm))
  }
}

case class Succ(t: Term) extends Term {
  override def toRawString = "succ(" + t.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    if (SimplyTyped.isNumericVal(this)) SimplyTyped.convertToNum(this).toString else surroundWithPar(par, "succ " + t)
}

case class Pred(t: Term) extends Term {
  override def toRawString = "pred " + t.toRawString
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = surroundWithPar(par, "pred " + t)
}

case class IsZero(t: Term) extends Term {
  override def toRawString = "iszero " + t.toRawString
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = surroundWithPar(par, "iszero " + t)
}

case class If(cond: Term, zen: Term, elze: Term) extends Term {
  override def toRawString = "if(" + cond.toRawString + ", " + zen.toRawString + ", " + elze.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = surroundWithPar(par, "if " + cond + " then " + zen + " else " + elze)
}

case class Pair(fst: Term, snd: Term) extends Term {
  override def toRawString = "pair(" + fst.toRawString + ", " + snd.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "{" + fst + "," + snd + "}"
}

case class First(p: Term) extends Term {
  override def toRawString = "fst(" + p.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = surroundWithPar(par, "fst " + p)
}

case class Second(p: Term) extends Term {
  override def toRawString = "snd(" + p.toRawString + ")"
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = surroundWithPar(par, "snd " + p)
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

