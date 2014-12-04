package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

import java.io.PrintWriter
import scala.collection.immutable.{ Map, ListMap }

import CT._
import Utils._

abstract class TreeException(msg: String) extends Exception

abstract class ClassException(msg: String) extends TreeException(msg)
case class ClassConstructorArgsException(msg: String) extends ClassException(msg)

abstract class MethodException(msg: String) extends TreeException(msg)
case class MethodArgsException(arg: String) extends MethodException(arg)
case class MethodOverrideException(msg: String) extends MethodException(msg)
case class MethodArgsLengthException(msg: String) extends MethodException(msg)

abstract class FieldException(msg: String) extends TreeException(msg)
case class FieldAlreadyDefined(msg: String) extends FieldException(msg)

sealed abstract class Tree extends Positional

case class Program(cls: List[ClassDef], expr: Expr) extends Tree {
  cls.foreach(c => CT.add(c.name, c))
}
case class ClassDef(name: String, superclass: String, fields: List[FieldDef], ctor: CtrDef, methods: List[MethodDef]) extends Tree {
  private def fieldLookup: List[FieldDef] = {
    CT lookup superclass match {
      case None => fields
      case Some(s) => s.fieldLookup ::: fields
    }
  }

  def getFieldsSuperclass: List[FieldDef] = getClassDef(superclass) fieldLookup

  def findField(fieldName: String): Option[FieldDef] = fieldLookup find (f => f.name == fieldName)

  def checkFields: Unit = checkListFieldsDef(fieldLookup)

  /**
   * Verify that in the list there is no two occurrence of the same variable name
   * Should throw FieldAlreadyDefined exception.
   */
  private def checkListFieldsDef(fs: List[FieldDef]): Unit = {
    val names = fs map { _.name }
    names diff names.distinct match {
      case Nil => ()
      case dups => throw FieldAlreadyDefined("variable(s) " + dups.mkString(", ") + " are already defined in the scope")
    }
  }

  def findMethod(methodName: String): Option[MethodDef] = {
    def zelf = methods find { _.name == methodName }
    def zuper = CT lookup superclass flatMap { _ findMethod methodName }
    zelf orElse zuper
  }

  def overrideMethod(tpe: String, name: String, args: List[FieldDef], body: Expr): Unit = {
    if ((methods count { _.name == name }) > 1)
      throw new MethodOverrideException(s"In class ${this.name}, method $name is defined more than once")

    try {
      checkListFieldsDef(args)
    } catch {
      case FieldAlreadyDefined(msg) =>
        throw FieldAlreadyDefined(s"In class ${this.name}, in the arguments of method $name, $msg")
    }

    val inheritedMethod = getClassDef(superclass) findMethod name
    inheritedMethod match {
      case None => ()
      case Some(MethodDef(tpeS, _, argsS, _)) =>
        if (tpe == tpeS) {
          val paramsOvMethod = args map { _.tpe }
          val paramsInMethod = argsS map { _.tpe }

          if (paramsOvMethod != paramsInMethod)
            throw new MethodOverrideException("can't apply " + paramsOvMethod.mkString("(", ",", ")") + " to " + paramsInMethod.mkString("(", ",", ")"))

          // Everything was ok, so override ok
        } else {
          throw new MethodOverrideException(s"Type mismatch. The return type $tpeS of inherithed method $name has different signature. Overriding method has type $tpe")
        }
    }
  }

  /**
   * checkTypeArguments: verify only the type of the parameters not the name
   * Should throw ClassConstructorArgsException's.
   */
  def checkTypeArguments(argsType: List[String]): Unit = {
    val typeFields: List[String] = fieldLookup map { _.tpe }

    if (typeFields.length == argsType.length) {
      for {
        (arg, field) <- argsType zip typeFields
        if !getClassDef(arg).isSubClassOf(field)
      } {
        throw new ClassConstructorArgsException("can't apply " + argsType.mkString("(", ",", ")") + " to " + typeFields.mkString("(", ",", ")") + " because " + arg + " is not a subtype of " + field)
      }

      // No errors means everything was fine... U DON'T SAY!
    } else {
      throw new ClassConstructorArgsException("can't apply " + argsType.mkString("(", ",", ")") + " to " + typeFields.mkString("(", ",", ")"))
    }
  }

  /**
   * verifyConstructorArgs: verify the name and the type of each parameter in the constructor respect to the fields declared in the class.
   * Should throw FieldAlreadyDefined and ClassConstructorArgsException.
   */
  def verifyConstructorArgs: Unit = {
    // NB: this test is potentially superfluous
    try {
      checkListFieldsDef(ctor.args) // checks only names!!
    } catch {
      case FieldAlreadyDefined(msg) => throw FieldAlreadyDefined(", in the constructor, " + msg)
    }

    // Check that all parameters and fields' names plus their respective types match exactly, in the same order
    val fieldss = fieldLookup
    if (fieldss != ctor.args) {
      throw new ClassConstructorArgsException("can't apply " + ctor.args.mkString("(", ",", ")") + " to " + fieldss.mkString("(", ",", ")"))
    }
  }

  def superClass: Option[ClassDef] = CT lookup superclass

  def isSuperclassOf(that: Option[ClassDef]): Boolean = {
    that map { clazz =>
      // C <: C, or 
      // CT(C) = class C extends D {...} -> C <: D OR C <: D & D <: E -> C <: E
      name == clazz.name || (this isSuperclassOf clazz.superClass)
    } getOrElse false
  }

  def isSubClassOf(that: ClassDef): Boolean = that isSuperclassOf Some(this)

  def isSubClassOf(that: String): Boolean = isSubClassOf(getClassDef(that))
}

case class FieldDef(tpe: String, name: String) extends Tree {
  override def toString = tpe + " " + name
}

case class CtrDef(name: String, args: List[FieldDef], supers: List[Var], body: List[Assign]) extends Tree

case class Assign(obj: String, field: String, rhs: Var) extends Tree

case class MethodDef(tpe: String, name: String, args: List[FieldDef], body: Expr) extends Tree {

  /*
   * Check type arguments of method definition. Should throw MethodArgsException's.
   */
  def checkTypeArguments(argsType: List[String]): Unit = {
    var error = false
    val params = args map { _.tpe }

    if (params.length != argsType.length)
      throw new MethodArgsException("can't apply " + argsType.mkString("(", ",", ")") + " to " + params.mkString("(", ",", ")"))

    for {
      (arg, param) <- argsType zip params
      if !(getClassDef(arg) isSubClassOf param)
    } {
      throw new MethodArgsException("can't apply " + argsType.mkString("(", ",", ")") + " to " + params.mkString("(", ",", ")"))
    }
  }
}

abstract class Expr extends Tree

case class Var(name: String) extends Expr {
  override def toString = name
}
case class New(cls: String, args: List[Expr]) extends Expr {
  override def toString = "new " + cls + "" + args.mkString("(", ",", ")")
}
case class Cast(cls: String, e: Expr) extends Expr {
  override def toString = "( (" + cls + ")" + e + ")"
}
case class Select(obj: Expr, field: String) extends Expr {
  override def toString = obj + "." + field
}
case class Apply(obj: Expr, method: String, args: List[Expr]) extends Expr {
  override def toString = obj + "." + method + "" + args.mkString("(", ",", ")")
}

/**
 * Pretty printer using scala.text formatting library. It works
 * by first building a document abstracting over nesting, spacing and
 * new lines, and afterwards rendering the text given a Writer and a
 * desired width.
 */
object PrettyPrinter {
  def apply(t: Tree) = {
    val writer = new PrintWriter(System.out)
    toDocument(t).format(80, writer)
    writer.println
    writer.flush()
  }

  import scala.text._
  import scala.text.Document._

  def toDocument(ts: List[Tree], sep: String, suffix: String): Document = ts match {
    case one :: two :: rest =>
      toDocument(one) :: sep :/: rest.foldLeft(toDocument(two)) { (d, e) =>
        if (sep != "") d :: sep :/: toDocument(e)
        else d :/: toDocument(e)
      } :: text(suffix)
    case one :: Nil =>
      toDocument(one) :: text(suffix)
    case Nil =>
      empty
  }

  def toDocument(t: Tree): Document = t match {
    case Program(cls, expr) =>
      group(toDocument(cls, "", "")) :/: group(toDocument(expr))

    case ClassDef(name, superclass, fields, ctor, methods) =>
      group("class " :: name :/: "extends " :: superclass :: empty) ::
        nest(2, " {" :/: group(toDocument(fields, ";", ";") :/: toDocument(ctor) :/: toDocument(methods, "", ""))) :/:
        "}" :/: empty

    case FieldDef(tpe, name) =>
      group(tpe :/: text(name))

    case CtrDef(name, args, supers, body) =>
      group(name :: "(" :: group(toDocument(args, ",", "")) :: ")" :: empty) :/:
        nest(2, "{" :/:
          group("super(" :: group(toDocument(supers, ",", "")) :: ");" :/: empty) :/:
          group(toDocument(body, "", ""))) :/:
        "}" :/: empty

    case Assign(obj, field, rhs) =>
      group(obj :: "." :: field :/: "=" :/: toDocument(rhs) :: ";" :/: empty)

    case MethodDef(tpe, name, args, body) =>
      group(tpe :/: name :/: "(" :: group(toDocument(args, ",", "")) :: ")" :/: text("{")) :/:
        nest(2, "return " :: group(toDocument(body))) :/:
        "}" :/: empty

    case Var(name) =>
      text(name)

    case New(cls, args) =>
      "new " :: cls :: "(" :: group(toDocument(args, ",", "") :: text(")"))
    case Cast(cls, expr) =>
      group("(" :: cls :: ")" :/: toDocument(expr))

    case Select(obj, field) =>
      toDocument(obj) :: "." :: text(field)

    case Apply(obj, meth, args) =>
      toDocument(obj) :: "." :: meth :: nest(2, "(" :: group(toDocument(args, ",", "")) :: text(")"))

    case _ =>
      super.toString :/: empty
  }
}
