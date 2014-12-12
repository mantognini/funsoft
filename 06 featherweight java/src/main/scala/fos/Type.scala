package fos

import scala.collection.mutable.{ Map, HashMap }

case class TypeError(msg: String) extends Exception(msg)

object Type {

  import CT._
  import Utils._

  type Class = String
  type Context = List[Pair[Class, String]]

  def typeOf(tree: Tree, ctx: Context): Class = ???
  //   ... To complete ... 
}

case class EvaluationException(msg: String) extends Exception
case class NoRuleApplies(expr: Expr) extends Exception(expr.toString)

object Value {
  def unapply(expr: Expr): Option[Expr] = expr match {
    case New(cls, Nil) => Some(expr)
    case New(cls, Values(args)) => Some(expr)
    case _ => None
  }
}

object Values {
  def unapply(xe: List[Expr]): Option[List[Expr]] = xe.find(_ match {
    case Value(_) => false
    case _ => true
  }) match {
    case None => Some(xe)
    case Some(_) => None
  }
}

object Evaluate extends (Expr => Expr) {

  import Utils._

  /*
   * Apply one step of the evaluation among rules (see TAPL p.258, Figure 19-3)
   * (1) E-ProjNew
   * (2) E-InvkNew
   * (3) E-CastNew
   * (4) E-Field
   * (5) E-Invk-Recv
   * (6) E-Invk-Arg
   * (7) E-New-Arg
   * (8) E-Cast
   */
  def apply(expr: Expr): Expr = expr match {
    case Var(name) => ??? // TODO: Implement evaluator for this expression
    case New(cls, args) => ??? // TODO: Implement evaluator for this expression
    case Cast(cls, e) => ??? // TODO: Implement evaluator for this expression
    case Select(obj, field) => obj match {
      case New(cls, Values(ctrArgs)) => getCstrArgValueFromField(cls, ctrArgs, field) // (1)
      case _ => Select(Evaluate(obj), field) // (4)
    }
    case Apply(obj, method, args) => (obj, args) match {
      case (Value(_), Values(_)) => ??? // TODO: (2)
      case (Value(_), _) => ??? // TODO: (6)
      case _ => Apply(Evaluate(obj), method, args) // (5)
    }
    case _ => throw NoRuleApplies(expr)
  }

  def substituteInBody(exp: Expr, thiss: New, substs: List[(FieldDef, Expr)]): Expr = exp match {
    case Select(obj: Expr, field: String) => Select(substituteInBody(obj, thiss, substs), field)
    case New(cls, args) => New(cls, args map (arg => substituteInBody(arg, thiss, substs)))
    case Cast(cls, e) => Cast(cls, substituteInBody(e, thiss, substs))
    case Var("this") => thiss
    case Var(bd) => substs find (subs => subs._1.name == bd) match {
      case None => exp
      case Some((_, sub)) => sub
    }

    case Apply(obj, method, args) => Apply(substituteInBody(obj, thiss, substs), method, args map (arg => substituteInBody(arg, thiss, substs)))
    case _ => throw new EvaluationException("Apply: Forgot expression " + exp)
  }

  def getCstrArgValueFromField(cls: String, ctrArgs: List[Expr], field: String): Expr = CT.lookup(cls) match {
    case Some(cd) => cd.fields.zip(ctrArgs).find(_._1.name == field) match {
      case Some((fd, ctrArg)) => ctrArg
      case None => throw new EvaluationException(s"Field access `(new $cls).$field` to an undefined class field")
    }
    case None => throw new EvaluationException(s"Field access `(new $cls).$field` to an undefined class")
  }
}

object CT {

  val objectClass: String = "Object"
  private val objectClassDef = ClassDef(objectClass, null, Nil, CtrDef(objectClass, Nil, Nil, Nil), Nil)

  private var ct: Map[String, ClassDef] = new HashMap[String, ClassDef]

  add(objectClass, objectClassDef)

  def elements = ct iterator

  def lookup(classname: String): Option[ClassDef] = if (classname != null) ct get classname else None

  def add(key: String, element: ClassDef): Unit = ct += key -> element

  def delete(key: String) = ct -= key

  def clear(): Unit = {
    ct.clear
    add(objectClass, objectClassDef)
  }

}

object Utils {

  def getClassDef(className: String): ClassDef = CT lookup className match {
    case None => throw new TypeError("class " + className + " not declared")
    case Some(c: ClassDef) => c
  }
}
