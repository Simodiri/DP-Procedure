import scala.collection.mutable.HashMap

object Solver{
	private def NB_DPaux(formula: Formula, assign: HashMap[Int,Boolean]):(Boolean,HashMap[Int,Boolean]) = {
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
		     if(f.isEmpty) (true, assignment)
		     else if(f contains E()) (false, assignment)
		     else{
				   var x=f.getVariables.head
		           val fp=Utils.branching(f,x,assignment)
		           val p = NB_DPaux(fp, assignment)
		          if(p._1) p
		          else{
					  (false,assignment)
				  }
		      
              } 
			 
	    }
	}
	def NB_DP(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		println("Esecuzione della NB_DP")
		val res = NB_DPaux(formula,Utils.buildAssignment(formula.numVariables))
		if(res._1) (true,formula.getResult(res._2))
		else (false, Nil)
	}
	
	
}


 
