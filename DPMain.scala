object DPMain extends App{
	//var f:Formula = new Formula(Set(U(1), C(Set(1,2,-3)), C(Set(-1,2,3,4)), C(Set(-5,-1)), C(Set(-5,-3)), U(4))) (soddisfacibile)
	//var f: Formula = new Formula(Set(C(Set(1,-2,4)), C(Set(-2,3,4)), C(Set(-1,2,-3,-4)), C(Set(2,-3))))//soddisfacibile (fa anche chiamata ricorsiva)

	var f:Formula = Parser.getFormula
	println(f)
	
	var res = Performance.profila(Solver.DPLL(f))
	if(res._1._1){
		println("Soddisfacibile con DPLL (Ha impiegato " + res._2*1E-9 + " sec).")
		println(res._1._2)
	}
	else{
		println("Non soddisfacibile.")
	}
	
	var res3 = Solver.DP(f)
    if(res3._1){
		println("Soddisfacibile con Davis Putnam")
		println(res3._2)
	}
	else{
		println("Non soddisfacibile con DP")
	} 
	
}
