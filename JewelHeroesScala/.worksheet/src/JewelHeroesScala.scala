object JewelHeroesScala {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(69); 
  println("Welcome to the Scala worksheet");$skip(765); 
  
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
  };System.out.println("""imprimir: (tablero: List[Int])Boolean""");$skip(336); 
  
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
	};System.out.println("""poner: (tablero: List[Int], color: Int, posicion: Int)List[Int]""");$skip(199); 
	
	/*
		EN: Generates an empty table.
		ES: Genera un tablero vacío.
	*/
	def generarv(longitud:Int): List[Int] = longitud match{
		case 0 => return Nil
		case longitud => 0::generarv(longitud-1)
	};System.out.println("""generarv: (longitud: Int)List[Int]""");$skip(354); 
  
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
  };System.out.println("""generarale: (tablero: List[Int])List[Int]""");$skip(38); 
  
  val lst:List[Int] = generarv(10);System.out.println("""lst  : List[Int] = """ + $show(lst ));$skip(28); val res$0 = 
  imprimir(generarale(lst));System.out.println("""res0: Boolean = """ + $show(res$0))}
}
