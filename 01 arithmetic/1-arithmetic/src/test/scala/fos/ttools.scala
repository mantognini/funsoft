package fos

object ttools {
  
	def testReduction(redFun: Term => (String => Unit) => Unit, testFun: String => String => Unit)
	(t: Term, expected: String): Unit = {
	  redFun(t)(testFun(expected))
	}
	
}