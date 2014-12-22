package fos

import scala.collection.mutable.{ Map => MutableMap, HashMap => MutableHashMap }

case class TypeError(msg: String) extends Exception(msg)

object Type {

  import CT._
  import Utils._

  type Class = String
  type Context = Map[String, Class] // List[Pair[Class, String]]
  val emptyContext: Context = Map.empty withDefault { name => throw TypeError(s"unknown variable $name in context") }

  implicit class ContextOps(ctx: Context) {
    type VarType = (String, Class)

    // Add one var with its type
    def ~(vt: VarType): Context = {
      ctx + (vt)
    }

    // Add fields def with their types
    def ~(vts: List[FieldDef]): Context = {
      ctx ++ (vts map { case FieldDef(tpe, name) => (name, tpe) })
    }
  }

  // Throws some exception if k is not in CT 
  implicit class ClassOps(k: Class) {
    val klass = (CT lookup k).get

    // Returns true iff k <: l
    // Throws if either k or l is not in CT
    // Note: `<:` is reserved so we use `<<=` to avoid clash and right associativity
    def <<=(l: Class): Boolean = {
      val llass = (CT lookup l).get
      klass isSubClassOf llass
    }

    // Get the fields name and type
    def getFields(): List[FieldDef] = klass fieldLookup

    // Get the method
    def getMethod(m: String) = klass findMethod m
  }

  // T-Method, assume klass is in CT
  // Throws MethodOverrideException or FieldAlreadyDefined
  def typecheckMethod(klass: ClassDef, md: MethodDef)(implicit ctx: Context) {
    val MethodDef(returnType, name, args, body) = md // extract info
    val exprType = typeOf(body)(ctx ~ ("this" -> klass.name) ~ args)
    if (!(exprType <<= returnType)) throw TypeError(s"return expression type mismatch, $returnType expected")
    klass.overrideMethod(returnType, name, args, body)
  }

  def typeOf(tree: Tree)(implicit ctx: Context): Class = tree match {
    // T-Class
    case klass @ ClassDef(thiz, _, fields, ctor, methods) =>
      // TODO should we check that ctx is empty?

      // Make sure the class is not being redefined
      if ((CT lookup thiz).isDefined) throw TypeError(s"redefinition of $thiz")

      try {
        /**
         * Check that everything is correct inside the class definition, i.e.:
         *  - the super class exists
         *  - no cyclic inheritance
         *  - no field shadowing
         *  - the constructor is valid, meaning:
         *    + the arguments match the fields (same names, same order, super's fields first)
         *    + the call to `super(...)` match the mother class' ctor
         *    + the "local" field are all initialised in the proper order
         *  - every method typechecks
         */
        def cyclicInheritance(opt: Option[ClassDef]) {
          opt map {
            case ClassDef(_, zuper, _, _, _) =>
              if (zuper == thiz) throw TypeError(s"Cyclic inheritance with $thiz")
              else cyclicInheritance(CT lookup zuper)
          }
        }

        klass.checkMotherIsAlive() // the super class was loaded
        cyclicInheritance(Some(klass)) // no cyclic inheritance
        klass.checkFields() // no shadowing
        klass.verifyConstructorArgs() // arguments' type and name match thiz and zuper's fields
        klass.ctor.check(klass); // proper call to super and valid initialisation of fields

        // We need to add the class to CT before typechecking the methods
        // since they can return a object of type klass.
        CT.add(thiz, klass)

        // Typecheck methods
        klass.methods foreach { typecheckMethod(klass, _) }

        // Everything was fine so we are entitled to add it to the CT
        // but we already did it earlier for method checking.
        // Also, if methods don't typecheck we have to remove klass from CT.
        // This is done below.

        thiz
      } catch {
        case e: TreeException =>
          // Cleanup and forward error
          CT.delete(thiz)
          throw TypeError(e.error)
      }

    // T-New
    case New(klass, args) =>
      val argsTypes = args map { typeOf(_) } // Check argument expressions
      val fieldsTypes = klass.getFields()
      for {
        (arg, field) <- argsTypes zip fieldsTypes
        if !(arg <<= field.tpe)
      } {
        // If any arguments is not a subclass a field we throw
        throw TypeError(s"$arg is not a subtype of $field in new statement $tree")
      }
      klass

    // T-Var
    case Var(name) =>
      ctx(name)

    // T-Field
    case Select(obj, field) =>
      val klass = typeOf(obj)
      val fieldDef = klass.getFields() find { _.name == field }
      val tpe = fieldDef map { _.tpe }
      tpe getOrElse { throw TypeError(s"no such field $field in $klass") }

    // T-Invk
    case Apply(obj, method, args) =>
      val klass = typeOf(obj)
      val methodDef = klass.getMethod(method) getOrElse { throw TypeError(s"no such method $method in $klass") }
      val paramsTypes = methodDef.args map { _.tpe }
      val argsTypes = args map { typeOf(_) } // Check argument expressions
      for {
        (arg, param) <- argsTypes zip paramsTypes
        if !(arg <<= param)
      } {
        // If any arguments is not a subclass a parameter we throw
        throw TypeError(s"$arg is not a subtype of $param in method invocation $tree")
      }
      methodDef.tpe // Return type of the method

    // T-UCast, T-DCast, T-SCast
    case Cast(klass, expr) =>
      val llass = typeOf(expr)
      if (llass <<= klass) klass // upcast
      else if ((klass <<= llass) && (klass != llass)) klass // downcast
      else if (!(klass <<= llass) && !(llass <<= klass)) {
        Console.err.println(s"WARNING: stupid cast from $llass to $klass")
        klass
      } else throw TypeError(s"invalid cast from $llass to $klass")

    case t =>
      throw new NotImplementedError(t.toString) // Same as `???` but with some debug info
  }
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
  private val objectClassDef = ClassDef(objectClass, null, Nil, CtorDef(objectClass, Nil, Nil, Nil), Nil)

  private var ct: MutableMap[Type.Class, ClassDef] = new MutableHashMap[String, ClassDef]

  add(objectClass, objectClassDef)

  def elements = ct iterator

  // `Object` is a class def with no method nor field and a simple ctor
  def lookup(classname: String): Option[ClassDef] = if (classname != null) ct get classname else None

  def add(key: Type.Class, element: ClassDef): Unit = ct += key -> element

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
