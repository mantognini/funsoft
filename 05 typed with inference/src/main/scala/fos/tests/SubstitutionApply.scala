package src.main.scala.fos.tests

object SubstitutionApply {
  /*
	 * Input STLC term (from statement - §3.5):
	 * 		\f.\x.f(f(x))
	 * may be parsed as
	 * 		t = \f:F.\x:X.f(f(x))
	 * which gives solution
	 *   ({F=X→T1, F=T1→T2}, F→(X→T2))
	 * which corresponds to the principal unifier
	 * 	[]°[T1↦T2]°[X↦T1]°[F↦(X→T1)]
	 *  
	 * TODO: test that applied to F→(X→T2), it gives type
	 * 	(T2→T2)→(T2→T2)
	 */
}