package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  val noConstraints: List[Constraint] = Nil
  case class TypingResult(tpe: Type, c: List[Constraint])

  /** Type <code>t</code> in <code>env</code> and return its type and a
   *  constraint list.
   */
  def collect(env: Env, t: Term): TypingResult = t match {
    case Var(x) =>
      val t1 = lookup(env, x)
      if (t1 == null)
        throw TypeError("Unknown variable " + x)
      TypingResult(t1.instantiate, noConstraints)
  //   ... To complete ... 
  }

  /**
   */
  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) emptySubst
    else c.head match {
      case (TypeVar(a), TypeVar(b)) if (a == b) =>
        unify(c.tail)
  //   ... To complete ... 
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }

  override def typeOf(t: Term): Type = try {
    val TypingResult(tp, c) = collect(Nil: Env, t)
    val s = unify(c)
    s(tp)
  } catch {
    case TypeError(msg) =>
      Console.println("type error: " + msg)
      null
  }

}
