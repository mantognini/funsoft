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
  // Returns None if `expr` is not a value
  def unapply(expr: Expr): Option[New] = expr match {
    case ne @ New(_, Nil) => Some(ne)
    case ne @ New(_, Values(args)) => Some(ne)
    case _ => None
  }
}

object Values {
  // Returns None if there exists one element in `xe` which is not a value.
  def unapply(xe: List[Expr]): Option[List[New]] =
    xe.map(Value.unapply(_)).foldRight(Some(Nil): Option[List[New]])((nArgOption, accu) => (nArgOption, accu) match {
      case (Some(nArg), Some(xn)) => Some(nArg :: xn)
      case _ => None
    })

  def existsANonValueIn(xs: List[Expr]): Boolean = unapply(xs).isEmpty
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
    case New(klass, args) if Values.existsANonValueIn(args) => // (7)
      args.span(Value.unapply(_).isDefined) match {
        case (values, nonValues) => New(klass, values ::: (Evaluate(nonValues.head) :: nonValues.tail))
      }
    case Cast(d, expr) => expr match {
      case New(c, Values(_)) if getClassDef(c).isSubClassOf(d) => expr // (3)
      case _ => Cast(d, Evaluate(expr)) // (8)
    }
    case Select(obj, field) =>
      obj match {
        case New(klass, Values(ctrArgs)) => getCtorArgValueFromField(klass, ctrArgs, field) // (1)
        case _ => Select(Evaluate(obj), field) // (4)
      }
    case Apply(obj, method, args) =>
      (obj, args) match {
        case (Value(vObj), Values(vArgs)) => { // (2)
          val md = getMethodDef(vObj.klass, method)
          substituteInBody(md.body, vObj, md.args.zip(vArgs))
        }
        case (Value(_), args) => //(6)
          args.span(Value.unapply(_).isDefined) match {
            case (values, nonValues) => Apply(obj, method, values ::: (Evaluate(nonValues.head) :: nonValues.tail))
          }
        case _ =>
          Apply(Evaluate(obj), method, args) // (5)
      }
    case _ => throw NoRuleApplies(expr)
  }

  def substituteInBody(exp: Expr, thiss: New, substs: List[(FieldDef, Expr)]): Expr = exp match {
    case Select(obj: Expr, field: String) => Select(substituteInBody(obj, thiss, substs), field)
    case New(klass, args) => New(klass, args map (arg => substituteInBody(arg, thiss, substs)))
    case Cast(klass, e) => Cast(klass, substituteInBody(e, thiss, substs))
    case Var("this") => thiss
    case Var(bd) => substs find (subs => subs._1.name == bd) match {
      case None => exp
      case Some((_, sub)) => sub
    }

    case Apply(obj, method, args) => Apply(substituteInBody(obj, thiss, substs), method, args map (arg => substituteInBody(arg, thiss, substs)))
    case _ => throw new EvaluationException("Apply: Forgot expression " + exp)
  }

  def getCtorArgValueFromField(klass: String, ctorArgs: List[Expr], field: String): Expr =
    getClassDef(klass).fields.zip(ctorArgs).find(_._1.name == field) match {
      case Some((fd, ctorArg)) => ctorArg
      case None => throw new EvaluationException(s"Cannot access field in `$klass(ctorArgs).$field`")
    }

  def getClassDef(klass: String): ClassDef =
    CT.lookup(klass) match {
      case Some(cd) => cd
      case None => throw new EvaluationException(s"Unknown class $klass")
    }

  def getFieldDef(klass: String, field: String): FieldDef =
    getClassDef(klass).fields.find(_.name == field) match {
      case Some(fd) => fd
      case None => throw new EvaluationException(s"Access `$klass.$field` to an undefined field")
    }

  def getMethodDef(klass: String, meth: String): MethodDef =
    getClassDef(klass).findMethod(meth) match {
      case Some(md) => md
      case None => throw new EvaluationException(s"Access `$klass.$meth` to an undefined method")
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
