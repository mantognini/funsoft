package src.main.scala.fos.tests

object Unify {
  /*
	 * Input STLC term (from statement - §3.5):
	 * 		\f.\x.f(f(x))
	 * may be parsed as
	 * 		t = \f:F.\x:X.f(f(x))
	 * which gives solution
	 *   ({F=X→T1, F=T1→T2}, F→(X→T2))
	 *   
	 * Unifying {F=X→T1, F=T1→T2} should produce (each step is one recursion down)
	 * 	(1) unify([F↦(X→T1)]{F=T1→T2})°[F↦(X→T1)]
	 *  (2) unify({X=T1,T1=T2})
	 *  (3) unify({T1=T2})°[X↦T1]
	 *  (4) unify({})°[T1↦T2]
	 *  (5) []
	 *  
	 * TODO: test that it gives the principal unifier
	 * 	[]°[T1↦T2]°[X↦T1]°[F↦(X→T1)]
	 */
}