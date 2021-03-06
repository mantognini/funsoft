package fos

abstract class TypeInferencers {
  import Type._

  case class TypeError(msg: String) extends Exception(msg)

  /** Lookup variable <code>name</code> in the given environment. */
  def lookup(env: Env, name: String): TypeScheme = env match {
    case Nil => null
    case (n, tp) :: env1 => if (n == name) tp else lookup(env1, name)
  }

  /** Turn a syntactic type (given explicitly) into a proper type. */
  def toType(s: TypeTree): Type = s match {
    case BoolTypeTerm => TypeBool
    case NatTypeTerm => TypeNat
    case FunTypeTerm(t1, t2) => TypeFun(toType(t1), toType(t2))
  }

  /** Return null when fails */
  def typeOf(t: Term): Type;
}
