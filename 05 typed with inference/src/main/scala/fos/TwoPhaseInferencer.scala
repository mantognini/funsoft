package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  // In order to be as close as possible to the rules given in Fig. 2
  // we define a few DSL facilities such as | or U.
  //
  // The following naming conventions for variables are used:
  //    T is noted tp, e.g. T1 becomes tp1
  //    C is noted c, e.g. C1 becomes c1
  //    {}, aka empty constraint, is denoted by Ø
  //    Γ is noted env
  //    Type constraint T1 = T2 is noted tp1 === tp2
  //    C1 ∪ C2 is noted c1 U c2
  //    X denotes a fresh type variable

  implicit class TypeDSL(tpe: Type) {
    // Usage: T | C
    def |(c: Constraint) = TypingResult(tpe, c :: Nil)
    def |(cs: List[Constraint]) = TypingResult(tpe, cs)

    // Function
    def ==>(to: Type) = TypeFun(tpe, to)

    // Constraint
    def ===(other: Type) = (tpe, other)
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
      val TypingResult(tp1, c1) = collect(t1)
      val c = tp1 === TypeNat U c1
      TypeNat | c

    case Succ(t1) =>
      val TypingResult(tp1, c1) = collect(t1)
      val c = tp1 === TypeNat U c1
      TypeNat | c

    case IsZero(t1) =>
      val TypingResult(tp1, c1) = collect(t1)
      val c = tp1 === TypeNat U c1
      TypeBool | c

    case If(t1, t2, t3) =>
      val TypingResult(tp1, c1) = collect(t1)
      val TypingResult(tp2, c2) = collect(t2)
      val TypingResult(tp3, c3) = collect(t3)
      val c = c1 U c2 U c3 U tp1 === TypeBool U tp2 === tp3
      tp2 | c

    case Var(v) =>
      val T = lookup(env, v)
      if (T == null)
        throw TypeError("Unknown variable " + v)
      T | Ø

    case Abs(arg, EmptyTypeTerm, t2) =>
      val x = Type.fresh
      val TypingResult(tp2, c) = collect(t2)((arg, x.toScheme()) :: env)
      x ==> tp2 | c

    case Abs(arg, tp, t2) =>
      val tp1 = toType(tp)
      val TypingResult(tp2, c) = collect(t2)((arg, tp1.toScheme()) :: env)
      tp1 ==> tp2 | c

    case App(t1, t2) =>
      val TypingResult(tp1, c1) = collect(t1)
      val TypingResult(tp2, c2) = collect(t2)
      val x = Type.fresh // is fresh
      val c = c1 U c2 U tp1 === (tp2 ==> x)
      x | c

    case Let(x, v, t) =>
      val TypingResult(tpv, cv) = collect(v)
      val substitution = unify(cv)
      val tp = substitution(tpv)
      val newEnv = substitution(env)
      val typeScheme = Type.generalize(tp, Type.collectTypeVars(newEnv))
      val TypingResult(tp2, c2) = collect(t)((x, typeScheme) :: newEnv)

      tp2 | (c2 U cv)
  }

  //  Substitution[X ↦ T] C
  def substitute(sub: (TypeVar, Type), c: List[Constraint]): List[Constraint] = {
    val (x, t) = sub

    def walk(tp: Type): Type = tp match {
      case tp if tp == x => t
      case TypeFun(a, b) => TypeFun(walk(a), walk(b))
      case tp => tp
    }
    c map { case (tp1, tp2) => (walk(tp1), walk(tp2)) }
  }

  // FV(T) is the set of all type variables mentioned in T. (§22.3.2, p.321)
  def FV(t: Type): Set[TypeVar] = t match {
    case TypeBool | TypeNat => Set.empty
    case v: TypeVar => Set(v)
    case TypeFun(a, b) => FV(a) ++ FV(b)
  }

  /** Unification Algorithm, TAPL p.327 */
  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) EmptySubstitution
    else c.head match {
      case (s, t) if (s == t) =>
        unify(c.tail)

      case (x: TypeVar, t) if !FV(t).contains(x) =>
        unify(substitute(x -> t, c.tail)) ° (x -> t)

      case (s, x: TypeVar) if !FV(s).contains(x) =>
        unify(substitute(x -> s, c.tail)) ° (x -> s)

      case (TypeFun(s1, s2), TypeFun(t1, t2)) =>
        unify(c.tail U s1 === t1 U s2 === t2)

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
