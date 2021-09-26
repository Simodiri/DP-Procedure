import scala.collection.mutable.HashMap

class Formula(clauses: Set[Clause], corr: List[(String,Int,String)]){
	def getClauses: Set[Clause] = { //fornisce le clausole
		this.clauses
	}
	
	def getCorr: List[(String,Int,String)] = { 
		this.corr
	}
	
	def getVariables: Set[String] = { 
		this.corr.filter(cor=>this.getClauses.exists(c=>c.getLiterals.contains(cor._2))).map(_._3).toSet
	}
	
	def isEmpty: Boolean = { //definisce se è vuota
		this.clauses.isEmpty
	}
	
	def numVariables: Int = { //numero di variabili della formula
		this.clauses.map(c => c.getLiterals).flatten.map(l => l.abs).size
	}
	
    override def toString: String = { //costruisce la formula
		this.clauses.map(c => c.print(this.corr)+" ∧\n ").reduce(_+_).dropRight(3)
	}
	
	def getVariable(l:Int):String={
		this.getCorr.filter(x=>x._2==l).map(_._3).head
	}
	
	def contains(c: Clause): Boolean = { //è true se contiene una determinata clausola
		this.clauses contains c
	}
	
	def containsUnitClauses: Boolean = { //definisce se la formula ha clausole unitarie
		this.clauses.filter(_.isUnit(this)).size >= 1
	}
	
	def toUnit: Formula = { //rende le clausole con un solo letterale unitarie
		new Formula(this.clauses.map(_.toUnit(this)), this.corr)
	}
	
	def getPureLiterals: (Set[Int], Set[Int]) = {
		val pos = this.clauses.map(c => c.filter(_ > 0)).flatten
		val neg = this.clauses.map(c => c.filter(_ < 0)).flatten
		val purepos = pos.filter(l => !(neg contains (-l)))
		val pureneg = neg.filter(l => !(pos contains (-l)))
		(purepos,pureneg)
	}
	
	def getLiterals: Set[Int] = { //dà in output i letterali della formula
		this.clauses.map(c => c.getLiterals).flatten
	}
	
	def removeClauses(clist: Set[Clause]):Formula = { //rimuove clausole
		new Formula(this.clauses -- clist, this.corr)
	}
	def removeClause(c: Clause):Formula = { //rimuove una sola clausola
		new Formula(this.clauses - c, this.corr)
	}
	
	def removeLiteralFromClauses(lit: Int):Formula = { //rimuove un letterale dalle clausole che lo contengono
		new Formula(this.clauses.map(c => c.removeLiteral(lit)), this.corr)
	}
	
	def removeLiteralsFromClauses(lit: Set[Int]):Formula = { //rimuove i letterali dalle clausole che li contengono
		new Formula(this.clauses.map(c => c.removeLiterals(lit)), this.corr)
	}
	
	def addClause(c: Clause):Formula = { //aggiunge una clausola
		new Formula(this.clauses + c, this.corr)
	}
	
	def getFirstUnitClause: Clause = { //dà in output la clausola unitaria
		this.clauses.filter(c => c.isUnit(this)).head
	}
	
	def chooseLiteral: Int = { //sceglie un letterale
		val literals = this.clauses.map(c => c.getLiterals).flatten.toList
		if(literals == Nil) 0
		else literals(0)
	}
	
	def getResult(assign: HashMap[Int,Boolean]):List[(String,Boolean)] = { //determina il risultato delle assegnazioni
		assign.map(a => (this.corr.filter(_._2 == a._1).head._1,a._2)).toList
	}
}

