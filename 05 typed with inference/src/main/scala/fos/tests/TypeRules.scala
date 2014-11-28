package src.main.scala.fos.tests

object TypeRules {
  /*
	 * Input STLC term (from statement - §3.5):
	 * 		\f.\x.f(f(x))
	 * may be parsed as
	 * 		t = \f:F.\x:X.f(f(x))
	 *   
	 * After applying typing rules:
	 * (1) (CT-Var to x) ^ (CT-Var to f)
	 * 		Γ = {x:X, f:F} ⊢ x:X | {}
	 * 		Γ = {x:X, f:F} ⊢ f:F | {}
	 *   
	 * (2) (CT-Var to f) ^ (CT-App to f(x))
	 * 		Γ = {x:X, f:F} ⊢ f:F | {}
	 * 		Γ = {x:X, f:F} ⊢ f(x):T1 | {F=X→T1}
	 *    		
	 * (3) (CT-App to f(f(x)))
	 * 		Γ = {x:X, f:F} ⊢ f(f(x)):T2 | {F=X→T1, F=T1→T2}
	 *   
	 * (4) (CT-Abs to \x:X.f(f(x)))
	 * 		Γ = {f:F} ⊢ \x:X.f(f(x)):X→T2 | {F=X→T1, F=T1→T2}
	 *   
	 * (5) (CT-Abs to \f:F.\x:X.f(f(x)))
	 * 		Γ = {f:F} ⊢ \f:F.\x:X.f(f(x)):F→(X→T2) | {F=X→T1, F=T1→T2}
	 *   
	 * TODO: test that typing t gives solution of form ({F=X→T1, F=T1→T2}, F→(X→T2))
	 */
}