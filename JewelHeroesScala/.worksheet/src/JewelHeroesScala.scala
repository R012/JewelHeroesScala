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
	};System.out.println("""poner: (tablero: List[Int], color: Int, posicion: Int)List[Int]""");$skip(274); 
	
	/*
		EN: Reads the element located in a specific location of the table.
		ES: Lee el elemento ubicado en una posición específica del tablero.
	*/
	def leer(tablero:List[Int], pos:Int):Int = pos match
	{
		case 0 => tablero.head
		case pos => leer(tablero.tail, pos-1)
	};System.out.println("""leer: (tablero: List[Int], pos: Int)Int""");$skip(199); 
	
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
  };System.out.println("""generarale: (tablero: List[Int])List[Int]""");$skip(400); 
  
  /*
  	EN: Exchanges the elements in two different spots of the table.
  	ES: Intercambia los elementos en dos puntos distintos del tablero.
  */
  def cambio(tablero:List[Int], pos0:Int, pos1:Int):List[Int] =
  {
  	val color0 = leer(tablero, pos0)
  	val color1 = leer(tablero, pos1)
  	val lista0 = poner(tablero, color1, pos0)
  	val lista1 = poner(lista0, color0, pos1)
  	return lista1
  };System.out.println("""cambio: (tablero: List[Int], pos0: Int, pos1: Int)List[Int]""");$skip(639); 
  
  /*
  	EN: Detects horizontal chains of diamonds with the same color
  	ES: Detecta cadenas horizontales de diamantes con el mismo color. (Necesario ampliar para
  	leer cadenas verticales.)
  */
  def leerDiamantes(tablero:List[Int], color:Int, pos:Int, cuenta:Int):List[Int] =
  {
  	if(color < 1) leerDiamantes(tablero.tail, tablero.head, pos+1, 1)
  	else if(pos%12 == 0){
  		if(cuenta >= 3) List(pos-cuenta, cuenta)
  		else List(-1,0)
  	}
  	else if(tablero.head == color) leerDiamantes(tablero.tail, color, pos+1, cuenta+1)
  	else if(cuenta>=3) List(pos-cuenta-1, cuenta)
  	else leerDiamantes(tablero.tail, 0, pos+1, 0)
  };System.out.println("""leerDiamantes: (tablero: List[Int], color: Int, pos: Int, cuenta: Int)List[Int]""");$skip(507); 
  
  /*
  	EN: Elimination function. Currently only erases rows, will soon erase columns.
  	ES: Función de eliminación. Ahora mismo solo elimina filas, en breve elminará columnas.
  */
  def borrar(tablero:List[Int], pos:Int, count:Int): Boolean =
  {
  	if(count <= 0) true
  	val random = scala.util.Random
  	if(pos < 12) {
  		poner(tablero, 1+random.nextInt(7), pos)
  		borrar(tablero, pos+1, count-1)
  		}
  		else {
  			cambio(tablero, pos, pos-12)
  			borrar(tablero, pos+1, count-1)
  		}
  };System.out.println("""borrar: (tablero: List[Int], pos: Int, count: Int)Boolean""");$skip(38); 
  
  val lst:List[Int] = generarv(10);System.out.println("""lst  : List[Int] = """ + $show(lst ));$skip(28); val res$0 = 
  imprimir(generarale(lst));System.out.println("""res0: Boolean = """ + $show(res$0))}
}
