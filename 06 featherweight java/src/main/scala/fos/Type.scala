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

  def apply(expr: Expr): Expr = expr match {
    case Var(name) => ??? // TODO: Implement evaluator for this expression
    case New(cls, args) => ??? // TODO: Implement evaluator for this expression
    case Cast(cls, e) => ??? // TODO: Implement evaluator for this expression
    case Select(obj, field) => ??? // TODO: Implement evaluator for this expression
    case Apply(obj, method, args) => ??? // TODO: Implement evaluator for this expression
    case _ => throw new EvaluationException(s"Forgot expression $expr")
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
