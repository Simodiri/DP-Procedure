import scala.collection.mutable.HashMap
sealed abstract class Clause{
	def isEmpty: Boolean = this match { //se la clausola è vuota
		case E() => true
		case _ => false
	}
	
        def map(f: Int => Any): List[(Int,Any)] = this match{ //applica la mappatura di Scala alle clasuole
		case E() => Nil
		case U(l) => List((l,f(l)))
		case C(v) => v.toList.map(l => (l,f(l)))
	}
	
	def filter(f: Int => Boolean): Set[Int] = this match{ //filtra dalle clausole quei letterali che verificano una condizione
		case E() => Set()
		case U(l) => if(f(l)) Set(l) else Set()
		case C(v) => v.filter(f(_))
	}
	
	def size: Int = this match { //dimensione clausola
		case E() => 0
		case U(l) => 1
		case C(v) => v.size
		case _ => 0
	}
	
	def toUnit: Clause = this match { //converte le clausole con un solo letterale in unitarie
		case C(v) if(v.size == 1) => U(v.head)
		case _ => this
	}
	
        def isUnit: Boolean = this match { //controlla se sia una clausola unitaria
		case U(l) => true
		case _ => false
	}
	
	def containsVariable(x: Int):Boolean = this match { //definisce se la clausola contiene una variabile
		case E() => false
		case U(l) => x == l.abs
		case C(v) => {
			val variables = v.map(_.abs)
			variables contains x
		}
	}

	def getLiterals:Set[Int] = this match{ //fornisce i letterali 
		case E() => Set()
		case U(l) => Set(l)
		case C(v) => v
	}
	
	def removeLiteral(lit: Int):Clause = this match{ //rimuove un letterale
		case E() => E()
		case U(l) => if(l == lit) E() else this
		case C(v) => C(v - lit)
	}
	
	def containsLiteral(lit: Int):Boolean = this match{ //verifica se la clausola contiene un letterale
		case E() => false
		case U(l) => l == lit
		case C(v) => v contains lit
	}
	
	def print(corr: List[(String,Int)]): String = this match { //costruisce le clausole
		case E() => "()"
		case U(l) => if(l > 0) "("+corr.filter(_._2 == l).head._1+")" 
		             else "(¬"+corr.filter(_._2 == -l).head._1+")"
		case C(v) => "(" + v.map(e => (if(e > 0) corr.filter(_._2 == e).head._1 
	                      else "¬"+corr.filter(_._2 == -e).head._1)+" ∨ ").reduce(_+_).dropRight(3) + ")"
	}
}
case class E() extends Clause				//clausola vuota
case class U(l: Int) extends Clause			//clausola unit
case class C(v: Set[Int]) extends Clause	//clausola composta da più literal	


	
	



