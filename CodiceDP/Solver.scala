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
				if(literal>0)println("Splitting su " +f.getCorr.filter(x=>x._2==literal).map(_._1).head)
				else println("Splitting su " +f.getCorr.filter(x=>x._2== -literal).map(_._1).head)
				val p = DPLLaux(fp, assignment)
				if(p._1) p
				else{
					DPLLaux(fn, assignment)
				}
			}
		}
	}
	def DPLL(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		println("Esecuzione della DPLL")
		val res = DPLLaux(formula,Utils.buildAssignment(formula.numVariables))
		if(res._1) (true,formula.getResult(res._2))
		else (false, Nil)
	}
	
	private def DPaux(formula: Formula):(Boolean) = {
		var f = formula.toUnit
		if(f.isEmpty) (true)
		else if(f contains E()) (false)
		else{
                         val up = Utils.dp_unitPropagation(f)  //unit propagation
			f = up
			
			val ple = Utils.dp_pureLiteralElimination(f)	//pure literal elimination
			f = ple
			
			val re=Utils.resolution(f) //resolution
			f = re
			
			DPaux(f)
		}
	}
	def DP(formula: Formula):(Boolean) = {
		println("Esecuzione della DP-Procedure")
		val res = DPaux(formula)
		if(res) (true)
		else (false)
	}
	
}


