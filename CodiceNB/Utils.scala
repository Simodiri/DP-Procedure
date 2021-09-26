import scala.collection.mutable.HashMap
object Utils{
	def unitPropagation(formula: Formula, assign:HashMap[Int,Boolean]):(Formula, HashMap[Int,Boolean]) = {
	   var f = formula
	   var assignment = assign
	   while(f.containsUnitClauses){
		  val unit = f.getFirstUnitClause
		  val unitlist = unit.getLiterals //lista dei letterali nella clausola unitaria 
		  var nbvar:String=f.getVariable(unitlist.head) //si prende la nbvariable
		  var nblitremained=assignment.filter(y=>f.getVariable(y._1)==nbvar)//variabili rimanenti 
		  for(lit<-unitlist){
		     if(lit > 0) assignment(lit) = true
		      else assignment(-lit) = false	 
		   }    
		   for(elem<-nblitremained){ //si assegnano gli altri valori a false
			  if(!unitlist.contains(elem._1)){
					assign(elem._1)=false
				}
		   }
	       f = f.removeClause(unit)
		   if(unitlist.size>1){
              var morelit = f.getClauses.filter(c => c.containsLiterals(unitlist) && !c.getLiterals.exists(x=>nbvar==f.getVariable(x) && !unitlist.contains(x))) //verifico che non ci siano altri letterali oltre a quello
			  f = f.removeClauses(morelit)  //si rimuovono le clausole che hanno solo quella coppia
			  f = f.removeLiteralsFromClauses(f.getLiterals.filter(x=>f.getVariable(x)==nbvar && !unitlist.contains(x))) //si rimuovono i letterali rimanenti
		   }else{
			  var onlylit=f.getClauses.filter(c => c.containsLiterals(unitlist))
			  f = f.removeClauses(onlylit)
			  f = f.removeLiteralsFromClauses(f.getLiterals.filter(x=>f.getVariable(x)==nbvar && !unitlist.contains(x)))
		   }
		   f = f.toUnit
		   if(!f.getClauses.isEmpty){
			   println("Unit Propagation su " +unit.print(f.getCorr)+": "+f)
		   }
		}
		(f, assignment)		
	}
	def pureLiteralElimination(formula: Formula, assign:HashMap[Int,Boolean]):(Formula, HashMap[Int,Boolean]) = {
		var (p,n) = formula.getPureLiterals
		var pos = p.toList
		var neg = n.toList
		var f = formula
		var assignment = assign
		var i = 0;
		var j = 0;
		if(!pos.isEmpty || !neg.isEmpty){
			for(i <- pos){
				var x:String=f.getVariable(i)
				var ccontainx=f.getClauses.filter(c=>c.getLiterals.exists(y=>f.getVariable(y)==x)) //clausole che contenegono la nbvar x
				if(ccontainx.forall(_.getLiterals.contains(i))){
			      assignment(i) = true
			      f = f.removeClauses(ccontainx)
			          if(!f.isEmpty){
			      println("Pure literal Elimination su " +f.getCorr.filter(x=>x._2==i).map(_._1).head+": "+f)
		         }
			    }
		    }
		    for(j <- neg){
			  var x:String=f.getVariable(j)
			  var ccontainx=f.getClauses.filter(c=>c.getLiterals.exists(y=>f.getVariable(y)==x)) //clausole che contenegono la nbvar x
			  if(ccontainx.forall(_.getLiterals.contains(j))){
			    assignment(-j) = false
			    var clpos = f.getClauses.filter(c => c.containsLiteral(i))
		    	  f = f.removeClauses(ccontainx)
		      }
		      if(!f.isEmpty){
			   println("Pure literal Elimination su ¬" +f.getCorr.filter(x=>x._2==j).map(_._1).head+": "+f)
		      }
		    }
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
   
    def branching(f:Formula,x:String, assign: HashMap[Int,Boolean]):Formula={
		var fo=f
		var assignment=assign
	        var nbliteralbranch=fo.getLiterals.filter(y=>fo.getVariable(y)==x)
		var lit=nbliteralbranch.head
		var nbvar=fo.getVariable(lit)
		var nblitremained=assignment.filter(y=>fo.getVariable(y._1)==nbvar)
		for(elem<-nblitremained){ //si assegnano gli altri valori a false
		      if(lit!=elem._1){
			 assign(elem._1)=false
		      }
	        }
		println("Variabile scelta "+f.getCorr.filter(_._2==lit).map(_._1))
	        var fp = new Formula(Set(U(Set(lit))) ++ fo.getClauses, fo.getCorr)
	        fp
    }
    def makeClause(ins:Set[Int]):Clause= { //vale solo per più di un letterale
	   C(ins)
	}
    
   
    
}
