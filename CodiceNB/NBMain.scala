object NBMain extends App{
	var f:Formula = Parser.getFormula
	println("Formula: " +f)
	println("NB-Variables " + f.getVariables) 
	var res = Solver.NB_DP(f)
	if(res._1){
		println("Soddisfacibile con NB_DP")
		println("Assegnazioni: "+printa(res._2,f))
		println("Assegnazioni alle variabili non boolean: "+printassign(res._2,f))
	}else{
		println("Non soddisfacibile")
	}
	def printa(assign:List[(String,Boolean)],f:Formula):String={
		var stringa=new String()
		assign.map(elem=>elem._1+":"+elem._2+",").reduce(_+_).dropRight(1)
	}
	def printassign(assign:List[(String,Boolean)],f:Formula):String={
		var stringa=new String()
		assign.filter(_._2==true).map(elem=>elem._1.substring(0,1)+"--->"+elem._1.substring(2,3)+",").reduce(_+_).dropRight(1)
	}
}
