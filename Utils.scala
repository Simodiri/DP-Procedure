import scala.collection.mutable.HashMap

object Utils{
	def unitPropagation(formula: Formula, assign:HashMap[Int,Boolean]):(Formula, HashMap[Int,Boolean]) = {
		var f = formula
		var assignment = assign
		while(f.containsUnitClauses){
			val unit = f.getFirstUnitClause
			val lit = unit.getLiterals.head
		    if(lit > 0) assignment(lit) = true
		    else assignment(-lit) = false	    
			f = f.removeClause(unit)
			var clit = f.getClauses.filter(c => c containsLiteral lit)
			f = f.removeClauses(clit)
			f = f.removeLiteralFromClauses(-lit)
			f = f.toUnit
			if(!f.getClauses.isEmpty){
			println("Unit Propagation "+ f)
		    }
		}
		(f, assignment)		
	}
	def pureLiteralElimination(formula: Formula, assign:HashMap[Int,Boolean]):(Formula, HashMap[Int,Boolean]) = {
		var (p,n) = formula.getPureLiterals
		var pos = p.toList
		var neg = n.toList
		println("positivi"+pos+"negativi"+neg)
		var f = formula
		var assignment = assign
		var i = 0;
		var j = 0;
		
		for(i <- pos){
			assignment(i) = true
			var clpos = f.getClauses.filter(c => c containsLiteral i)
			
			f = f.removeClauses(clpos)
			
			//println("Pure literal Elimination su ¬" +f.getCorr.filter(x=>x._2==j).map(_._1).head+": "+f)
		}
		for(j <- neg){
			assignment(-j) = false
			
			var clneg = f.getClauses.filter(c => c containsLiteral j)
			f = f.removeClauses(clneg)
			
		}
			println("Pure literal Elimination")
		(f.toUnit, assignment)		
	}
	def buildAssignment(n: Int):HashMap[Int,Boolean] = {
		var i = 1;
		var assignment = new HashMap[Int, Boolean](n,1)
		while(i <= n){
			assignment.addOne(i -> false)
			i += 1
		}
		assignment
	}
	
    def resolve(f:Formula, assign:HashMap[Int,Boolean]):(Formula, HashMap[Int,Boolean])={
		var fo=f
		//var list=fo.getClauses.filter(a=>a.getLiterals.exists(x=>if(x<0)fo.getClauses.exists(y=>y.containsLiteral(x)) else fo.getClauses.exists(y=>y.containsLiteral(-x)))).toList //mi da le clausole che hanno x e 
			//println(list)
		var assignment=assign
		var littomerge=Set[Int]()
		if(fo.getLiterals.exists(x=>f.getLiterals.exists(_==(-x)))){
			 var x=fo.getLiterals.filter(x=>f.getLiterals.exists(_==(-x))).head
		     var nocc=fo.getClauses.filter(a=>a.containsLiteral(-x)).toSet //clausole che contengono l'opposto
			 var occ=fo.getClauses.filter(a=>a.containsLiteral(x)).toSet    //clausole che contengono il letterale
		        if(!occ.isEmpty && !nocc.isEmpty){
					println(x)
					/* println("Occ "+occ)
					 println("nocc "+nocc)
					 println("Clauses formula "+fo.getClauses)*/
							    if(x > 0) assignment(x) = true
		                        else assignment(-x) = false	 
				             	littomerge=littomerge ++ Utils.resolution(occ,nocc,x) //prende i letterali da mettere insieme
				             	fo=fo.addClause(makeClause(littomerge))				   
				             	fo=fo.removeClauses(occ)
                                 fo=fo.removeClauses(nocc)	
                   }		
                     littomerge=Set[Int]()
                     if(!fo.getClauses.isEmpty){
	        	println("Resolution "+fo)
		       }
							
		}
			
		
	
	    (fo.toUnit,assignment)
	}
	
	def resolution(r:Set[Clause],c:Set[Clause],lit:Int):Set[Int]={
		var totlet=Set[Int]()
		for(x<-r){
			var a=x.removeLiteral(lit)
			totlet=totlet ++a.getLiterals 
		}
		for(y<-c){
			var b=y.removeLiteral(-lit)
			totlet=totlet ++b.getLiterals 
		}
		totlet
		
    }
    
    def makeClause(ins:Set[Int]):Clause= { //vale solo per più di un letterale
	   C(ins)
	}
   
    
}
