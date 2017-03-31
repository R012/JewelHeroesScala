object JewelHeroesScala {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  /*
  	EN: Function that prints the table to the screen. Still need to add a way to skip lines.
  	ES: Función que escribe el tablero por pantalla. Todavía hay que añadir una forma de saltar
  	líneas.
  */
  def imprimir(tablero:List[Int]) : Boolean ={
  	if(tablero.isEmpty) return true
  	tablero.head match{
  		case 1 => print("A\t")
  		 imprimir(tablero.tail)
  		case 2 => print("R\t")
  		 imprimir(tablero.tail)
  		case 3 => print("N\t")
  		 imprimir(tablero.tail)
  		case 4 => print("V\t")
  		 imprimir(tablero.tail)
  		case 5 => print("P\t")
  		 imprimir(tablero.tail)
  		case 6 => print("M\t")
  		 imprimir(tablero.tail)
  		case 7 => print("G\t")
  		 imprimir(tablero.tail)
  		case 8 => print("B\t")
  		 imprimir(tablero.tail)
  	}
  }                                               //> imprimir: (tablero: List[Int])Boolean
  
  /*
  	EN: Inserts element in a given position of the table.
  	ES: Inserta un element en una posición dada del tablero.
  */
  def poner(tablero:List[Int], color:Int, posicion:Int) :List[Int] =
	{
		posicion match{
			case 0 => color::tablero.tail
  		case posicion => tablero.head::poner(tablero.tail, color, posicion - 1)
		}
	}                                         //> poner: (tablero: List[Int], color: Int, posicion: Int)List[Int]
	
	/*
		EN: Generates an empty table.
		ES: Genera un tablero vacío.
	*/
	def generarv(longitud:Int): List[Int] = longitud match{
		case 0 => return Nil
		case longitud => 0::generarv(longitud-1)
	}                                         //> generarv: (longitud: Int)List[Int]
  
  /*
  	EN: Inserts random number into the table.
  	ES: Inserta valores aleatorios en el tablero. Falta comprobar el nivel de dificultad para
  	definir el rango de valores.
  */
  def generarale(tablero:List[Int]):List[Int] =
  {
  	if(tablero.isEmpty) return Nil
  	val rand = scala.util.Random
  	(rand.nextInt(7)+1)::generarale(tablero.tail)
  }                                               //> generarale: (tablero: List[Int])List[Int]
  
  val lst:List[Int] = generarv(10)                //> lst  : List[Int] = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  imprimir(generarale(lst))                       //> G	P	G	R	M	P	V	M	P	N	
                                                  //| res0: Boolean = true
}