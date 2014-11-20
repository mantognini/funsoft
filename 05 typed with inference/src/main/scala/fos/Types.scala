package fos

import scala.collection.immutable.{Set, ListSet}

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => "(" + a + " -> " + b + ")"
    case TypeNat => "Nat"
    case TypeBool => "Bool"
  }
}

case class TypeVar(name: String) extends Type
  //   ... To complete ... 

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  //   ... To complete ... 
  override def toString() = args.mkString("[", ", ", "].") + tp
}

object Type {
  //   ... To complete ... 
}

abstract class Substitution extends (Type => Type) {

  var indent = 0

  //   ... To complete ... 
  def apply(tp: Type): Type = {
    //println("  " * indent + "in: " + tp + "   subst: " + this)
    indent = indent + 1
    val result = tp match {
  //   ... To complete ... 
    }
    indent = indent - 1
    //println("  " * indent + "out: " + result + "   subst: " + this)
    result
  }
  override def toString() = ""

  def apply(p: (Type, Type)): (Type, Type) = p match {
    case Pair(t1, t2) => (this(t1), this(t2))
  }

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] =
    env map { (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp))) }

  //   ... To complete ... 
}

/** The empty substitution. */
object emptySubst extends Substitution {
  def lookup(t: TypeVar) = t
}
