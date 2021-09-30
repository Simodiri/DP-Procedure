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
				if(lit>0)println("Unit Propagation su (" +f.getCorr.filter(x=>x._2==lit).map(_._1).head+"): "+f)
				else println("Unit Propagation su (¬" +f.getCorr.filter(x=>x._2== -lit).map(_._1).head+"): "+f)
			
		    }
		}
		(f, assignment)		
	}
    def dp_unitPropagation(formula: Formula):(Formula) = {
		var f = formula
		while(f.containsUnitClauses){
			val unit = f.getFirstUnitClause
			val lit = unit.getLiterals.head    
			f = f.removeClause(unit)
			var clit = f.getClauses.filter(c => c containsLiteral lit)
			f = f.removeClauses(clit)
			f = f.removeLiteralFromClauses(-lit)
			f = f.toUnit
			if(!f.getClauses.isEmpty){
			 if(lit>0)println("Unit Propagation su (" +f.getCorr.filter(x=>x._2==lit).map(_._1).head+"): "+f)
				else println("Unit Propagation su (¬" +f.getCorr.filter(x=>x._2== -lit).map(_._1).head+"): "+f)
		    }
		}
		f		
	}
	
	def pureLiteralElimination(formula: Formula, assign:HashMap[Int,Boolean]):(Formula, HashMap[Int,Boolean]) = {
		var (p,n) = formula.getPureLiterals
		var pos = p.toList
		var neg = n.toList
		//println("positivi"+pos+"negativi"+neg)
		var f = formula
		var assignment = assign
		var i = 0;
		var j = 0;
		if(!pos.isEmpty || !neg.isEmpty){
			
		    for(i <- pos){
			  assignment(i) = true
		   	  var clpos = f.getClauses.filter(c => c containsLiteral i)
			  f = f.removeClauses(clpos)
			  if(!f.isEmpty){
			   println("Pure literal Elimination su " +f.getCorr.filter(x=>x._2==i).map(_._1).head+": "+f)
		    }
		    }
		    for(j <- neg){
			  assignment(-j) = false
			  var clneg = f.getClauses.filter(c => c containsLiteral j)
			  f = f.removeClauses(clneg)
			  if(!f.isEmpty){
			   println("Pure literal Elimination su ¬" +f.getCorr.filter(x=>x._2==j).map(_._1).head+": "+f)
		      }
		    }
		   
	    }
		(f.toUnit, assignment)		
	}
	
	def dp_pureLiteralElimination(formula: Formula):(Formula) = {
		var (p,n) = formula.getPureLiterals
		var pos = p.toList
		var neg = n.toList
		//println("positivi"+pos+"negativi"+neg)
		var f = formula
		var i = 0;
		var j = 0;
		if(!pos.isEmpty || !neg.isEmpty){
		  for(i <- pos){
			var clpos = f.getClauses.filter(c => c containsLiteral i)
			f = f.removeClauses(clpos)
			if(!f.isEmpty){
			   println("Pure literal Elimination su " +f.getCorr.filter(x=>x._2==i).map(_._1).head+": "+f)
		    }
		  }
		  for(j <- neg){
			var clneg = f.getClauses.filter(c => c containsLiteral j)
			f = f.removeClauses(clneg)	
			if(!f.isEmpty){
			   println("Pure literal Elimination su ¬" +f.getCorr.filter(x=>x._2==j).map(_._1).head+": "+f)
		    }
		  }
	    }
		f.toUnit	
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
	
    def resolution(f:Formula):(Formula)={
		var fo=f
        if(fo.getLiterals.exists(x=>f.getLiterals.exists(_==(-x)))){
			 var x=fo.getLiterals.filter(x=>f.getLiterals.exists(_==(-x))).head
		     var nocc=fo.getClauses.filter(a=>a.containsLiteral(-x)).toSet //clausole che contengono l'opposto
			 var occ=fo.getClauses.filter(a=>a.containsLiteral(x)).toSet    //clausole che contengono il letterale
		     if(occ==nocc){
				 fo=fo.removeClauses(occ)
				 occ=Set()
				 nocc=Set()
			 }
		     if(!occ.isEmpty && !nocc.isEmpty){
                  for(c<-occ){
					 for(r<-nocc){
					   fo=fo.addClause(Utils.resolve(c,r,x))
					 }
				  }
                    fo=fo.removeClauses(occ)
                    fo=fo.removeClauses(nocc)	
              }
			 if(!fo.getClauses.isEmpty){
	        	println("Resolution su "+f.getCorr.filter(y=>y._2==x.abs).map(_._1).head+": "+fo)
		      }
		}
	    fo.toUnit
     }	

	def resolve(r:Clause,c:Clause,lit:Int):Clause={     //  Set[Int]   r:Set[Clause],c:Set[Clause],
		var totlet=Set[Int]()
		    var a=r.removeLiteral(lit)
			if(a.containsLiteral(-lit)){
				 a=a.removeLiteral(-lit)
			}
			totlet=totlet ++a.getLiterals 
		
			var b=c.removeLiteral(-lit)
			if(b.containsLiteral(lit)){
				 b=b.removeLiteral(lit)
			}
			totlet=totlet ++b.getLiterals 
		makeClause(totlet)
	}
    
    def makeClause(ins:Set[Int]):Clause= { //vale solo per più di un letterale
	   C(ins)
	}
   
    
}
