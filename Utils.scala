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
		}
		for(j <- neg){
			assignment(-j) = false
			var clneg = f.getClauses.filter(c => c containsLiteral j)
			f = f.removeClauses(clneg)
	        }
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
	
     def resolve(f:Formula):(Formula)={
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
			   if(occ.size >=2 && nocc.size ==1 ){
				 for(c<-occ){
					 fo=fo.addClause(Utils.resolution(c,nocc.head,x))
					 fo=fo.removeClause(c)     
				 }
				 fo=fo.removeClause(nocc.head)	
			   }else if(nocc.size >=2 && occ.size ==1 ){
				 for(c<-nocc){
					 fo=fo.addClause(Utils.resolution(occ.head,c,x))
					 fo=fo.removeClause(c)
				 }
				 fo=fo.removeClause(occ.head)
			  }else if(nocc.size >=2 && occ.size >=2 ){
				 for(c<-occ){
					 for(r<-nocc){
					   fo=fo.addClause(Utils.resolution(c,r,x))
					   
                                         }
                                  }
                                  fo=fo.removeClauses(occ)
                                  fo=fo.removeClauses(nocc)	
                          }else{
				   fo=fo.addClause(Utils.resolution(occ.head,nocc.head,x))
				   fo=fo.removeClauses(occ)
                                   fo=fo.removeClauses(nocc)
			  }
		       }
		}
	    fo.toUnit
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
