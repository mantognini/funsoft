package fos

import scala.collection.immutable.{ Set, ListSet }

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => "(" + a + " -> " + b + ")"
    case TypeNat => "Nat"
    case TypeBool => "Bool"
  }

  // Convert the type to a generic scheme 
  def toScheme(args: List[TypeVar] = Nil) = TypeScheme(args, this)
}

case class TypeVar(name: String) extends Type
case class TypeFun(from: Type, to: Type) extends Type
case object TypeNat extends Type
case object TypeBool extends Type

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  def instantiate = if (args.isEmpty) tp else ??? // TODO non empty type scheme
  override def toString() = args.mkString("[", ", ", "].") + tp
}

object Type {
  // TODO what should be added to Type?

  private var last = 0 // id counter for fresh var names
  def fresh: TypeVar = {
    last += 1
    TypeVar("tp$" + last)
  }
}

abstract class Substitution extends (Type => Type) {

  var indent = 0

  def apply(tp: Type): Type = {
    //println("  " * indent + "in: " + tp + "   subst: " + this)
    indent = indent + 1
    val result = tp match {
      case t @ TypeVar(v) => lookup(t) match {
        case t2 @ TypeVar(w) if v != w => this(t2)
        case TypeFun(a, b) => TypeFun(this(a), this(b))
        case t2 => t2
      }
      case TypeFun(from, to) => TypeFun(apply(from), apply(to))
      case TypeNat => TypeNat
      case TypeBool => TypeBool
    }
    indent = indent - 1
    //println("  " * indent + "out: " + result + "   subst: " + this)
    result
  }

  def apply(p: (Type, Type)): (Type, Type) = p match {
    case Pair(t1, t2) => (this(t1), this(t2))
  }

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] =
    env map { (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp))) }

  def lookup(t: TypeVar): Type;

  // Composition
  def °(s: (TypeVar, Type)) = NonEmptySubstitution(s._1, s._2, this)
}

/** The empty substitution. */
object EmptySubstitution extends Substitution {
  def lookup(t: TypeVar): Type = t

  override def toString() = "Ø"
}

/**
 * The non empty substitution, with its tail
 */
case class NonEmptySubstitution(from: TypeVar, to: Type, tail: Substitution) extends Substitution {
  def lookup(t: TypeVar): Type = if (t == from) to else tail.lookup(t)

  override def toString() = s"[ $from -> $to ] ° $tail"
}
