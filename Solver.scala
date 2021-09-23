import scala.collection.mutable.HashMap

object Solver{
	private def DPLLaux(formula: Formula, assign: HashMap[Int,Boolean]):(Boolean,HashMap[Int,Boolean]) = {
		var f = formula.toUnit
		var assignment = assign
		if(f.isEmpty) (true, assignment)
		else if(f contains E()) (false, assignment)
		else{
			val up = Utils.unitPropagation(f, assignment)	//unit propagation
			f = up._1
			assignment = up._2
			
			val ple = Utils.pureLiteralElimination(f, assignment)	//pure literal elimination
			f = ple._1
			assignment = ple._2
			val literal = f.chooseLiteral	//choose literal
			if(f.isEmpty) (true, assignment)
			else if(f contains E()) (false, assignment)
			else{
				var fp = new Formula(Set(U(literal.abs)) ++ f.getClauses, f.getCorr)
				var fn = new Formula(Set(U(-literal.abs)) ++ f.getClauses, f.getCorr)
				val p = DPLLaux(fp, assignment)
				if(p._1) p
				else{
					DPLLaux(fn, assignment)
				}
			}
		}
	}
	def DPLL(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		val res = DPLLaux(formula, Utils.buildAssignment(formula.numVariables))
		if(res._1) (true, formula.getResult(res._2))
		else (false, Nil)
	}
	
	private def DPaux(formula: Formula, assign: HashMap[Int,Boolean]):(Boolean,HashMap[Int,Boolean]) = {
		var f = formula.toUnit
		var assignment = assign
		if(f.isEmpty) (true, assignment)
		else if(f contains E()) (false, assignment)
		else{
			val up = Utils.unitPropagation(f, assignment)	//unit propagation
			f = up._1
			assignment = up._2
			println
			val ple = Utils.pureLiteralElimination(f, assignment)	//pure literal elimination
			f = ple._1
			assignment = ple._2
			val re=Utils.resolution(f,assignment)
			f = re._1
			assignment = re._2
			DPaux(f,assignment)
		}
	
	}
	def DP(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		val res = DPaux(formula, Utils.buildAssignment(formula.numVariables))
		
		if(res._1) (true, formula.getResult(res._2))
		else (false, Nil)
	}
}


