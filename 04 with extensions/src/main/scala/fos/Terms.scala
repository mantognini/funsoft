package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false): String
  final override def toString = prettyString()

  final def surroundWithPar(par: Boolean, msg: String): String = {
    def open = if (par) "(" else ""
    def close = if (par) ")" else ""
    s"$open$msg$close"
  }
}

case class True() extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "true"
}

case class False() extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "false"
}
case class Zero() extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "0"
}

case class Var(name: String) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = name
}

case class Abs(x: Var, typ: Type, body: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, s"\\$x:$typ.$body")
}

case class App(t1: Term, t2: Term) extends Term {
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
    case Fix(t) => true
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
    val t1Prettyfied = t1.prettyString(lpar, cornerCase)
    val t2Prettyfied = t2.prettyString(rpar || forceRighParInInnerTerm)

    surroundWithPar(par, s"$t1Prettyfied $t2Prettyfied")
  }
}

case class Succ(t: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, s"succ $t")
}

case class Pred(t: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, s"pred $t")
}

case class IsZero(t: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, s"iszero $t")
}

case class If(cond: Term, zen: Term, elze: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, s"if $cond then $zen else $elze")
}

case class Pair(fst: Term, snd: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    s"{$fst,$snd}"
}

case class First(p: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, s"fst $p")
}

case class Second(p: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, s"snd $p")
}

case class Fix(t: Term) extends Term {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) =
    surroundWithPar(par, s"fix $t")
}

case class Inl(t: Term, typ: Type) extends Term {
  // TODO
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = ""
}

case class Inr(t: Term, typ: Type) extends Term {
  // TODO
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = ""
}

case class Case(caseTerm: Term, inlVar: Var, inlBody: Term, inrVar: Var, inrBody: Term) extends Term {
  // TODO
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = ""
}

/** Abstract Syntax Trees for types. */
abstract class Type extends Term {
  /* For pattern matching only */
  protected object ComposedType {
    def unapply(t: Type) = t match {
      case Product(_, _) => Some(t)
      case Function(_, _) => Some(t)
      case _ => None
    }
  }
}

case class Bool() extends Type {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "Bool"
}

case class Nat() extends Type {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = "Nat"
}

case class Function(i: Type, o: Type) extends Type {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = (i, o) match {
    case (Product(_, _), _) => s"$i->$o"
    case (ComposedType(_), _) => s"($i)->$o"
    case _ => s"$i->$o"
  }
}

case class Product(fst: Type, snd: Type) extends Type {
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = (fst, snd) match {
    case (ComposedType(_), Function(_, _)) => s"($fst)*($snd)"
    case (_, Function(_, _)) => s"$fst*($snd)"
    case (ComposedType(_), _) => s"($fst)*$snd"
    case _ => s"$fst*$snd"
  }
}

case class Sum(typ1: Type, typ2: Type) extends Type {
  // TODO
  override def prettyString(par: Boolean = false, forceRighParInInnerTerm: Boolean = false) = ""
}

