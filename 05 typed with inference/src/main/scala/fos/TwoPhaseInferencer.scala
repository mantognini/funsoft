package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  // In order to be as close as possible to the rules given in Fig. 2
  // we define a few DSL facilities such as | or U.
  //
  // NB: we intentionally break naming consistency of variables,
  // such as T and C (instead of name starting with a lower case letter). 

  implicit class TypeDSL(tpe: Type) {
    // Usage: T | C
    def |(c: Constraint) = TypingResult(tpe, c :: Nil)
    def |(cs: List[Constraint]) = TypingResult(tpe, cs)
  }

  implicit class TypeSchemeDSL(scheme: TypeScheme) {
    // Usage: T | C
    def |(c: Constraint) = TypingResult(scheme.instantiate, c :: Nil)
    def |(cs: List[Constraint]) = TypingResult(scheme.instantiate, cs)
  }

  implicit class ConstraintDSL(c1: Constraint) {
    // Union
    def U(cs: List[Constraint]) = c1 :: cs
  }

  implicit class ConstraintsDSL(cs: List[Constraint]) {
    // Union
    def U(cs2: List[Constraint]) = cs ::: cs2
    def U(c: Constraint) = c :: cs
  }

  // Empty constrain set
  val Ø: List[Constraint] = Nil
  case class TypingResult(tpe: Type, c: List[Constraint])

  /**
   * Type <code>t</code> in <code>env</code> and return its type and a
   *  constraint list.
   */
  def collect(t: Term)(implicit env: Env): TypingResult = t match {

    case True() =>
      TypeBool | Ø

    case False() =>
      TypeBool | Ø

    case Zero() =>
      TypeNat | Ø

    case Pred(t1) =>
      val tr1 = collect(t1)
      val C = (tr1.tpe, TypeNat) U tr1.c
      TypeNat | C

    case Succ(t1) =>
      val tr1 = collect(t1)
      val C = (tr1.tpe, TypeNat) U tr1.c
      TypeNat | C

    case IsZero(t1) =>
      val tr1 = collect(t1)
      val C = (tr1.tpe, TypeNat) U tr1.c
      TypeBool | C

    case If(t1, t2, t3) =>
      val tr1 = collect(t1)
      val tr2 = collect(t2)
      val tr3 = collect(t3)
      val T1 = tr1.tpe
      val T2 = tr2.tpe
      val T3 = tr3.tpe
      val C = tr1.c U tr2.c U tr3.c U (T1, TypeBool) U (T2, T3)
      T2 | C

    case Var(x) =>
      val T = lookup(env, x)
      if (T == null)
        throw TypeError("Unknown variable " + x)
      T | Ø

    case Abs(x, EmptyTypeTerm, t2) =>
      ???

    case Abs(x, tp1, t2) =>
      ???

    case App(t1, t2) =>
      ???

    case Let(x, v, t) =>
      ???
  }

  /** Unification Algorithm, TAPL p.327 */
  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) EmptySubstitution
    else c.head match {
      case (TypeVar(a), TypeVar(b)) if (a == b) =>
        unify(c.tail)
      // TODO implement TwoPhaseInferencer.unify
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }

  override def typeOf(t: Term): Type = try {
    val TypingResult(tp, c) = collect(t)(Nil)
    val s = unify(c)
    s(tp)
  } catch {
    case TypeError(msg) =>
      Console.println("type error: " + msg)
      null
  }

}
