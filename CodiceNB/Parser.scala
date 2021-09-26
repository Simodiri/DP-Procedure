object Parser{
	def getFormula: Formula= {
		var clausestoret = Set[Clause]() //insieme delle clausole per creare la formula
		var clauses = Set[Set[String]]() 
		var ok = true
	    var nbelem:List[String]=List()
		var domain:List[String]=List() //dominio
		var nblit:Set[String]=Set()
		var nbvar=new String()
		var containsempty = false
		println("Inserisci le NB-variables (in maiuscolo) con il relativo dominio (ad esempio X(a,b,c)), ognuna separata da uno spazio (' ')")
		var nbinput = scala.io.StdIn.readLine()
		var nb:List[String] = nbinput.split(' ').toList //lista di variabili con dominio
		for(elem<-nb){
			 nbelem=elem.split(Array('(',')')).toList
			 nbvar=nbelem.head.toString
			 domain=nbelem.last.toString.split(',').toList.map(c=>nbvar+"_"+c)
			 for(elem<-domain){
				nblit=nblit+elem //lista dei letterali
			 }
	    } 
	    
		println("Scrivi i letterali delle clausole della formula")
		while(ok){
			println("Inserisci una clausola della formula con i literal sottoforma di X_a (X è la variabile e a il valore) separati da uno spazio (' ') oppure 0 per concludere: ")
			var input = scala.io.StdIn.readLine()
			if(input == "0")
				ok = false
		    else{
				var lit:List[String] = input.split(' ').toList
				var clause = Set[String]()
				if(lit contains ""){
					ok = false
					containsempty = true
				}
				else{
					var i = ' '
					for(i <- lit){
						if(nblit.contains(i)){
						 clause = clause + i
						}else{
						    println(i+" non appartiene al dominio della variabile")
						    ok=false	
						    new Formula(Set(E()), Nil)
						}
					}
			     	clauses = clauses + clause
				}
			}
		}
		
		if(containsempty){
			println("La formula che hai inserito contiene una clausola vuota quindi è insoddisfacibile")
			new Formula(Set(E()), Nil)
		}
		else{
			var variables = clauses.flatten.map(l => if(l.head == '-') l.tail else l)
			var corr = variables.toList.zipWithIndex.map(t => (t._1, t._2+1,t._1.substring(0,1)))
			if(corr == Nil){
				new Formula(Set(E()), Nil)
			}
			else{
				var clausesInt = clauses.map(c => c.map(l => if(l.head == '-') corr.filter(_._1 == l.tail).map(-_._2).head else corr.filter(_._1 == l).map(_._2).head))
				clausestoret = clausesInt.map(e => if(e.isEmpty) E() else C(e))
				new Formula(clausestoret, corr).toUnit
			}
		}
	}
}

