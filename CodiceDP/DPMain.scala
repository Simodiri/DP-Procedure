
object DPMain extends App{
	var f:Formula = Parser.getFormula
	println("Formula in input: " +f)
	var res = Solver.DPLL(f)
	if(res._1){
		println("Soddisfacibile con DPLL")
		
		println("Assegnazioni: "+printa(res._2,f))
	}
	else{
		println("Non soddisfacibile con DPLL.")
	}
	var res3 = Solver.DP(f)
        if(res3){
		println("Soddisfacibile con Davis Putnam")
	}else{
		println("Non soddisfacibile con DP")
	} 
	
	def printa(assign:List[(String,Boolean)],f:Formula):String={
		var stringa=new String()
		assign.map(elem=>elem._1+":"+elem._2+",").reduce(_+_).dropRight(1)
        }
}
 
