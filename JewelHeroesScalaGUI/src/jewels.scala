import java.io._;
import javax.swing._
import java.awt.Color
import java.awt.GridLayout

object jewels {
   
  /*
  	EN: Function that prints the table to the screen.
  	ES: Función que escribe el tablero por pantalla.
  */
  def imprimir(tablero:List[Int], dificultad:Int,pos:Int) : Boolean ={
  	if(tablero.isEmpty) return true
  	//Determina si hay que saltar de línea
  	dificultad match{
  	    case 1 =>
  	        if(pos>=7){
  	        val a = pos%7
  	        a match{case 0 => print("\n")
  	        case a=>print("")}
  	        }
  	    case 2 =>
  	        if(pos>=11){
  	        val a = pos%11
  	        a match{case 0 => print("\n")
  	        case a=>print("")}
  	        }
  	    case 3 =>
  	        if(pos>=15){
  	        val a = pos%15
  	        a match{case 0 => print("\n")
  	        case a=>print("")}
  	        }
  	}
  	//Escribe el caracter actual
  	tablero.head match{
  		case 1 => print("A\t")
  		 imprimir(tablero.tail,dificultad,pos+1)
  		case 2 => print("R\t")
  		 imprimir(tablero.tail,dificultad,pos+1)
  		case 3 => print("N\t")
  		 imprimir(tablero.tail,dificultad,pos+1)
  		case 4 => print("V\t")
  		 imprimir(tablero.tail,dificultad,pos+1)
  		case 5 => print("P\t")
  		 imprimir(tablero.tail,dificultad,pos+1)
  		case 6 => print("M\t")
  		 imprimir(tablero.tail,dificultad,pos+1)
  		case 7 => print("G\t")
  		 imprimir(tablero.tail,dificultad,pos+1)
  		case 8 => print("B\t")
  		 imprimir(tablero.tail,dificultad,pos+1)
  	}
  }
  
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
	}
	
	/*
		EN: Reads the element located in a specific location of the table.
		ES: Lee el elemento ubicado en una posición específica del tablero.
	*/
	def leer(tablero:List[Int], pos:Int):Int = pos match
	{
		case 0 => tablero.head
		case pos => leer(tablero.tail, pos-1)
	}
	
	/*
		EN: Generates an empty table.
		ES: Genera un tablero vacío.
	*/
	def generarv(longitud:Int): List[Int] = longitud match{
		case 0 => return Nil
		case longitud => 0::generarv(longitud-1)
	}
  
  /*
  	EN: Inserts random number into the table.
  	ES: Inserta valores aleatorios en el tablero.
  */
  def generarale(tablero:List[Int], dificultad:Int):List[Int] =
  {
  	if(tablero.isEmpty) return Nil
  	val rand = scala.util.Random
  	//Determina los posibles valores en función de la dificultad
  	dificultad match{
  		case 1 => (rand.nextInt(3)+1)::generarale(tablero.tail, dificultad)
  		case 2 => (rand.nextInt(4)+1)::generarale(tablero.tail, dificultad)
  		case 3 => (rand.nextInt(8)+1)::generarale(tablero.tail, dificultad)
  	}
  }
  
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
  }
  
  /*
  	EN: Detects chains of diamonds with the same color
  	ES: Detecta cadenas de diamantes con el mismo color. (Necesario ampliar para
  	leer cadenas verticales.)
  */
  def leerDiamantes(tablero:List[Int], color:Int, pos:Int, cuenta:Int, col:Boolean, dificultad:Int):(Int,Int) =
  {
  //Utiliza la dificultad para determinar cómo iterar
  dificultad match{
  case 1 =>{
		  if(!col){	//Comprueba is está iterando verticalmente
		  	//Determina si existe una cadena válida
		  	if(color < 1) leerDiamantes(tablero.tail, tablero.head, pos+1, 1, col, dificultad)
			  	else if(pos>=7 && pos%7 == 0){
			  		if(cuenta >= 3) (pos-cuenta, cuenta)
			  		else (-1,0)
			  	}
			  	else if(tablero.head == color) leerDiamantes(tablero.tail, color, pos+1, cuenta+1, col, dificultad)
			  	else if(cuenta>=3) (pos-cuenta-1, cuenta)
			  	else leerDiamantes(tablero.tail, 0, pos+1, 0, col, dificultad)
		  	}
		  	else
		  	{
		  		//Determina si existe una cadena válida
		  		if(color < 1) leerDiamantes(tablero.tail, tablero.head, pos+7, 1, col, dificultad)
		  		else if(pos>=9 && pos%9 == 0){
			  		if(cuenta >= 3) (pos-cuenta, cuenta)
			  		else (-1,0)
			  	}
			  	else if(tablero.head == color) leerDiamantes(tablero.tail, color, pos+7, cuenta+1, col, dificultad)
			  	else if(cuenta>=3) (pos-cuenta-1, cuenta)
			  	else leerDiamantes(tablero.tail, 0, pos+7, 0, col, dificultad)
		  	}
	  	}
	  	case 2 =>{
	  	//Determina si está iterando por filas o columnas
		  if(!col){
		  //Determina si existe una cadena válida
		  	if(color < 1) leerDiamantes(tablero.tail, tablero.head, pos+1, 1, col, dificultad)
			  	else if(pos>=11 && pos%11 == 0){
			  		if(cuenta >= 3) (pos-cuenta, cuenta)
			  		else (-1,0)
			  	}
			  	else if(tablero.head == color) leerDiamantes(tablero.tail, color, pos+1, cuenta+1, col, dificultad)
			  	else if(cuenta>=3) (pos-cuenta-1, cuenta)
			  	else leerDiamantes(tablero.tail, 0, pos+1, 0, col, dificultad)
		  	}
		  	else
		  	{
		  	//Determina si existe una cadena válida
		  		if(color < 1) leerDiamantes(tablero.tail, tablero.head, pos+11, 1, col, dificultad)
		  		else if(pos >= 17 && pos%17 == 0){
			  		if(cuenta >= 3) (pos-cuenta, cuenta)
			  		else (-1,0)
			  	}
			  	else if(tablero.head == color) leerDiamantes(tablero.tail, color, pos+11, cuenta+1, col, dificultad)
			  	else if(cuenta>=3) (pos-cuenta-1, cuenta)
			  	else leerDiamantes(tablero.tail, 0, pos+11, 0, col, dificultad)
		  	}
	  	}
	  	case 3 =>{
	  	//Determina si está iterando por filas o columnas
		  if(!col){
		  //Determina si existe una cadena válida
		  	if(color < 1) leerDiamantes(tablero.tail, tablero.head, pos+1, 1, col, dificultad)
			  	else if(pos>=15 && pos%15 == 0){
			  		if(cuenta >= 3) (pos-cuenta, cuenta)
			  		else (-1,0)
			  	}
			  	else if(tablero.head == color) leerDiamantes(tablero.tail, color, pos+1, cuenta+1, col, dificultad)
			  	else if(cuenta>=3) (pos-cuenta-1, cuenta)
			  	else leerDiamantes(tablero.tail, 0, pos+1, 0, col, dificultad)
		  	}
		  	else
		  	{
		  	//Determina si existe una cadena válida
		  		if(color < 1) leerDiamantes(tablero.tail, tablero.head, pos+15, 1, col, dificultad)
		  		else if(pos >= 27 && pos%27 == 0){
			  		if(cuenta >= 3) (pos-cuenta, cuenta)
			  		else (-1,0)
			  	}
			  	else if(tablero.head == color) leerDiamantes(tablero.tail, color, pos+15, cuenta+1, col, dificultad)
			  	else if(cuenta>=3) (pos-cuenta-1, cuenta)
			  	else leerDiamantes(tablero.tail, 0, pos+15, 0, col, dificultad)
		  	}
	  	}
  	}
  }
  
  /*
  	EN: Elimination function.
  	ES: Función de eliminación.
  */
  def borrar(tablero:List[Int], pos:Int, count:Int, col:Boolean, dificultad:Int): Boolean =
  {
  //Comprueba si el bucle ha concluido
  	if(count <= 0) true
  	val random = scala.util.Random
  	//Itera empleando la dificultad como referencia. Desplaza filas o columnas, y recrea al llegar al extremo
  	dificultad match{
  	case 1 =>{
		  	if(!col)
		  	{
			  	if(pos < 7) {
			  		poner(tablero, 1+random.nextInt(3), pos)
			  		borrar(tablero, pos+1, count-1,col, dificultad)
			  		}
			  		else {
			  			cambio(tablero, pos, pos-7)
			  			borrar(tablero, pos+1, count-1,col, dificultad)
			  		}
		  		}
		  		else
		  		{
		  			if(pos < 9) {
				  		poner(tablero, 1+random.nextInt(3), pos)
				  		borrar(tablero, pos+7, count-1,col, dificultad)
			  		}
			  		else {
			  			cambio(tablero, pos, pos-1)
			  			borrar(tablero, pos+7, count-1,col, dificultad)
			  		}
		  		}
	  		}
	  		case 2 =>{
		  	if(!col)
		  	{
			  	if(pos < 11) {
			  		poner(tablero, 1+random.nextInt(3), pos)
			  		borrar(tablero, pos+1, count-1,col, dificultad)
			  		}
			  		else {
			  			cambio(tablero, pos, pos-11)
			  			borrar(tablero, pos+1, count-1,col, dificultad)
			  		}
		  		}
		  		else
		  		{
		  			if(pos < 17) {
				  		poner(tablero, 1+random.nextInt(4), pos)
				  		borrar(tablero, pos+11, count-1,col, dificultad)
			  		}
			  		else {
			  			cambio(tablero, pos, pos-1)
			  			borrar(tablero, pos+11, count-1,col, dificultad)
			  		}
		  		}
	  		}
	  		case 3 =>{
		  	if(!col)
		  	{
			  	if(pos < 15) {
			  		poner(tablero, 1+random.nextInt(8), pos)
			  		borrar(tablero, pos+1, count-1,col, dificultad)
			  		}
			  		else {
			  			cambio(tablero, pos, pos-15)
			  			borrar(tablero, pos+1, count-1,col, dificultad)
			  		}
		  		}
		  		else
		  		{
		  			if(pos < 27) {
				  		poner(tablero, 1+random.nextInt(8), pos)
				  		borrar(tablero, pos+15, count-1,col, dificultad)
			  		}
			  		else {
			  			cambio(tablero, pos, pos-1)
			  			borrar(tablero, pos+15, count-1,col, dificultad)
			  		}
		  		}
	  		}
  		}
  }
  
  
  /*
  	EN: Function that searches for the best possible move and returns a tuple representing it.
  	ES: Función que busca el mejor movimiento posible y devuelve una tupla que lo representa.
  */
  def buscarMejor(tablero:List[Int], inicio: Int, longitud: Int, color:Int, col: Boolean, dificultad:Int):(Int,Int,Int)=
  {
  //Emplea la dificultad para iterar por el tablero. Se trata de un sencillo algoritmo de back-tracking que
  //determina qué movimiento resultará en la cadena de mayor longitud, priorizando la creación de cadenas
  //horizontales.
  dificultad match{
  case 1 =>{
	  	if(leer(tablero,inicio) != color)
			{
				if(!col)
				{
					if(inicio > 7)
					{
						if(leer(tablero,inicio-7) == color) (inicio-7,1,longitud+1)
						else if(inicio/7 < 9)
						 	if(leer(tablero,inicio+7) == color) (inicio+7,0,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
					}
					else if(inicio/7 < 9)
						 	if(leer(tablero,inicio+7) == color) (inicio+7,0,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
				}
				else
				{
					if(inicio/7 > 0)
					{
						if(leer(tablero,inicio-1) == color) (inicio-1,3,longitud+1)
						else if(inicio/9 < 7)
						 	if(leer(tablero,inicio+1) == color) (inicio+1,2,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
					}
					else if(inicio/9 < 7)
						 	if(leer(tablero,inicio+1) == color) (inicio+1,2,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
				}
			}
	  	else
	  	{
	  		if(!col)
	  		{
	  			if(color!=0)
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,longitud+1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+7,1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  			else
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,longitud+1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+7,1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  		}
	  		else
	  		{
	  			if(color!=0)
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+7,longitud+1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  			else
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+7,longitud+1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  		}
	  		}
  }
	  	case 2=>{
	  	if(leer(tablero,inicio) != color)
			{
				if(!col)
				{
					if(inicio > 11)
					{
						if(leer(tablero,inicio-11) == color) (inicio-11,1,longitud+1)
						else if(inicio/11 < 17)
						 	if(leer(tablero,inicio+11) == color) (inicio+11,0,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
					}
					else if(inicio/11 < 17)
						 	if(leer(tablero,inicio+11) == color) (inicio+11,0,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
				}
				else
				{
					if(inicio/11 > 0)
					{
						if(leer(tablero,inicio-1) == color) (inicio-1,3,longitud+1)
						else if(inicio/17 < 11)
						 	if(leer(tablero,inicio+1) == color) (inicio+1,2,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
					}
					else if(inicio/17 < 11)
						 	if(leer(tablero,inicio+1) == color) (inicio+1,2,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
				}
			}
	  	else
	  	{
	  		if(!col)
	  		{
	  			if(color!=0)
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,longitud+1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+11,1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  			else
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,longitud+1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+11,1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  		}
	  		else
	  		{
	  			if(color!=0)
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+11,longitud+1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  			else
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+11,longitud+1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  		}
	  		}}
	  		case 3 =>{
	  	if(leer(tablero,inicio) != color)
			{
				if(!col)
				{
					if(inicio > 15)
					{
						if(leer(tablero,inicio-15) == color) (inicio-15,1,longitud+1)
						else if(inicio/15 < 27)
						 	if(leer(tablero,inicio+15) == color) (inicio+15,0,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
					}
					else if(inicio/15 < 27)
						 	if(leer(tablero,inicio+15) == color) (inicio+15,0,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
				}
				else
				{
					if(inicio/15 > 0)
					{
						if(leer(tablero,inicio-1) == color) (inicio-1,3,longitud+1)
						else if(inicio/27 < 15)
						 	if(leer(tablero,inicio+1) == color) (inicio+1,2,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
					}
					else if(inicio/27 < 15)
						 	if(leer(tablero,inicio+1) == color) (inicio+1,2,longitud+1)
						 	else(-1,-1,-1)
						else(-1,-1,-1)
				}
			}
	  	else
	  	{
	  		if(!col)
	  		{
	  			if(color!=0)
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,longitud+1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+15,1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  			else
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,longitud+1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+15,1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  		}
	  		else
	  		{
	  			if(color!=0)
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+15,longitud+1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  			else
	  				{
	  					val hor = buscarMejor(tablero,inicio+1,1,color,false, dificultad)
	  					val ver = buscarMejor(tablero,inicio+15,longitud+1,color,true, dificultad)
	  					if(hor._2 > ver._2) hor
	  					else ver
	  					}
	  		}
	  	}
	  	}
  	}
  }
  
  /*
  	EN: Function that saves current game state to a file.
  	ES: Función que guarda el estado actual del juego en un archivo.
  */
  def guardar(fname:String, tablero:List[Int],dificultad:Int,automatico:Int) =
  {
  	val out = new ObjectOutputStream(new FileOutputStream(fname))
  	out.writeObject((tablero,dificultad,automatico))
  	out.close()
  }
  
  /*
  	EN: Function that loads a previously loaded game state from a file.
  	ES: Functión que carga un estado de juego previamente guardado de un archivo.
  */
  def cargar(fname:String):(List[Int],Int,Int)=
  {
  	val in = new ObjectInputStream(new FileInputStream(fname))
  	val res = in.readObject()
  	in.close()
  	res.asInstanceOf[(List[Int],Int,Int)]
  }
  
  /*
  	EN: Function that does as many eliminations as possible.
  	ES: Función que hace tantas eliminaciones como sea posible.
  */
  def eliminarPosibles(tablero:List[Int],col:Boolean, dificultad:Int) : Boolean =
  {
  	val eliminacion = leerDiamantes(tablero,0,0,0,col,dificultad)
  	if(eliminacion == (-1,-1)) false
  	else
  	{
  		borrar(tablero,eliminacion._1,eliminacion._2,col, dificultad)
  		eliminarPosibles(tablero,col,dificultad)
  	}
  }
  
    
  /*
   EN: Function that creates the grid view of the game
   ES: Función que crea la cuadrícula visual del juego
   */
  def crearGUI(ancho:Int, alto:Int): JFrame = {
    val f = new JFrame("Jewel Heroes")
    val grid = new GridLayout(ancho, alto)
    f.setLayout(grid)
    inicializarGUI(0,ancho*alto,f)
    f.setSize(800, 800)
    f.setVisible(true)
    f
  }
  
  /*
    EN: Function initializing the grid view of the game with a blank grid
    ES: Función que inicializa la cuadrícula del juego con una vista vacía
   */
  def inicializarGUI(pos:Int, hasta:Int, f:JFrame) {
    if(pos<=hasta){
      val sq = new JPanel()
      f.add(sq)
      inicializarGUI(pos+1,hasta,f)
    }
  }
  
  /*
    EN: Function that updates the jewels' colours according to the current game status
    ES: Función que actualiza los colores de las jewels de acuerdo al estado del juego
   */
  def actualizarGUI(tablero:List[Int], pos:Int, f:JFrame) {
    if(!tablero.isEmpty){
      f.remove(pos)
      val sq = new JPanel()
      tablero.head match {
        case 1 => sq.setBackground(Color.blue)
        case 2 => sq.setBackground(Color.red)
        case 3 => sq.setBackground(Color.orange)
        case 4 => sq.setBackground(Color.green)
        case 5 => sq.setBackground(Color.gray)
        case 6 => sq.setBackground(Color.magenta)
        case 7 => sq.setBackground(Color.darkGray)
        case 8 => sq.setBackground(Color.white)
      }
      f.add(sq,pos)
      actualizarGUI(tablero.tail, pos+1, f)
    }
    else {
      f.validate()
      f.repaint()
    }
  }
  
  /*
  	EN: Main game loop. Makes as many eliminations as possible, then proceeds to save the game state,
  	determine the next move and execute it.
  	ES: Bucle principal de juego. Realiza tantas eliminaciones como sea posible, y a continuación
  	procede a guardar el estado del juego, determinar el siguiente movimiento y ejecutarlo.
  */
  def bucle(tablero:List[Int],dificultad:Int,automatico:Int, gui: JFrame):Boolean=
  {
  	eliminarPosibles(tablero,false,dificultad)
  	eliminarPosibles(tablero,true,dificultad)
  	guardar("save.dat",tablero,dificultad,automatico)
  	dificultad match{
  	case 1 =>{
	  	automatico match{
	  		case 1 =>{
	  			val mov = buscarMejor(tablero,0,0,0,false,dificultad)
	  			mov._2 match{
	  				case 0 =>{
	  				 val nuevoTablero = cambio(tablero,mov._1,mov._1-7)
	  				 print(nuevoTablero, dificultad, 0)
	  				 actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				 }
	  				case 1 =>{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+7)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 2 =>
	  				{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1-1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 3 => {
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  			}
	  			}
	  		case automatico =>{
	  			println("Inserte posicion X para el movimiento (esquina superior izquierda es 0,0):")
	  			val x = readInt()
	  			println("Inserte posicion Y para el movimiento (esquina superior izquierda es 0,0):")
	  			val y = readInt()
	  			println("Inserte el movimiento que desea hacer:\n\t0.Hacia arriba\n\t1.Hacia abajo\n\t2.Hacia la izquierda\n\t3.Hacia la derecha")
	  			val dir = readInt()
	  			val mov = (x*y,dir)
	  			mov._2 match{
	  				case 0 =>{
	  				 val nuevoTablero = cambio(tablero,mov._1,mov._1-7)
	  				 print(nuevoTablero, dificultad, 0)
	  				 actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				 }
	  				case 1 =>{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+7)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 2 =>
	  				{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1-1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 3 => {
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  			}
	  		}
	  		case 2 =>{
	  			val mov = buscarMejor(tablero,0,0,0,false,dificultad)
	  			mov._2 match{
	  				case 0 =>{
	  				 val nuevoTablero = cambio(tablero,mov._1,mov._1-11)
	  				 print(nuevoTablero, dificultad, 0)
	  				 actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				 }
	  				case 1 =>{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+11)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 2 =>
	  				{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1-1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 3 => {
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  			}
	  			}
	  		case automatico =>{
	  			println("Inserte posicion X para el movimiento (esquina superior izquierda es 0,0):")
	  			val x = readInt()
	  			println("Inserte posicion Y para el movimiento (esquina superior izquierda es 0,0):")
	  			val y = readInt()
	  			println("Inserte el movimiento que desea hacer:\n\t0.Hacia arriba\n\t1.Hacia abajo\n\t2.Hacia la izquierda\n\t3.Hacia la derecha")
	  			val dir = readInt()
	  			val mov = (x*y,dir)
	  			mov._2 match{
	  				case 0 =>{
	  				 val nuevoTablero = cambio(tablero,mov._1,mov._1-11)
	  				 print(nuevoTablero, dificultad, 0)
	  				 actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				 }
	  				case 1 =>{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+11)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 2 =>
	  				{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1-1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 3 => {
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  			}
	  		}
	  		case 3 =>{
	  			val mov = buscarMejor(tablero,0,0,0,false,dificultad)
	  			mov._2 match{
	  				case 0 =>{
	  				 val nuevoTablero = cambio(tablero,mov._1,mov._1-15)
	  				 print(nuevoTablero, dificultad, 0)
	  				 actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				 }
	  				case 1 =>{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+15)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 2 =>
	  				{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1-1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 3 => {
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  			}
	  			}
	  		case automatico =>{
	  			println("Inserte posicion X para el movimiento (esquina superior izquierda es 0,0):")
	  			val x = readInt()
	  			println("Inserte posicion Y para el movimiento (esquina superior izquierda es 0,0):")
	  			val y = readInt()
	  			println("Inserte el movimiento que desea hacer:\n\t0.Hacia arriba\n\t1.Hacia abajo\n\t2.Hacia la izquierda\n\t3.Hacia la derecha")
	  			val dir = readInt()
	  			val mov = (x*y,dir)
	  			mov._2 match{
	  				case 0 =>{
	  				 val nuevoTablero = cambio(tablero,mov._1,mov._1-15)
	  				 print(nuevoTablero, dificultad, 0)
	  				 actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				 }
	  				case 1 =>{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+15)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 2 =>
	  				{
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1-1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  				case 3 => {
	  					val nuevoTablero = cambio(tablero,mov._1,mov._1+1)
	  					print(nuevoTablero, dificultad, 0)
	  					actualizarGUI(nuevoTablero,0,gui)
	  				 bucle(nuevoTablero,dificultad,automatico,gui)
	  				}
	  			}
	  		}
	  		}
  }}
  }
  
  /*
  	EN: Function used to quickly get whether a game is auto or not.
  	ES: Función utilizada para determinar rápidamente si una partida es automática o no.
  */
  def esAutomatico():Int=
  {
  	println("¿Desea jugar en modo automático?\n\t1.Sí\n\t2.No")
  	readInt()
  }
  
  /*
  	EN: Init function that retrieves the basic game configuration from the user.
  	ES: Función de inicialización que recupera la configuración básica del juego del usuario.
  */
  def init()=
  {
    println("Bienvenido a Jewels Hero.\nSeleccione lo que quiere hacer:\n\t1.Nueva partida\n\t2.Cargar partida")
  	val opcion = readInt()
  	opcion match
  	{
  		case 1 =>
  			println("Elige tu nivel de dificultad:\n\t1.Fácil\n\t2.Medio\n\t3.Difícil")
			  val dificultad = readInt()
			  dificultad match{
			    case 1 => {
			      val lst:List[Int] = generarv(7*9) //7x9
			      val tablero = generarale(lst,dificultad)
			      val gui = crearGUI(7,9)
			      bucle(tablero,dificultad,esAutomatico(),gui)
			    }
			    case 2 => {
			      val lst:List[Int] = generarv(11*17) //11x17
			      val tablero = generarale(lst,dificultad)
			      val gui = crearGUI(11,17)
			      bucle(tablero,dificultad,esAutomatico(),gui)
			    }
			    case 3 => {
			      val lst:List[Int] = generarv(15*27) //15x27
			      val tablero = generarale(lst,dificultad)
			      val gui = crearGUI(15,27)
			      bucle(tablero,dificultad,esAutomatico(),gui)
			    }
			    case default => print("Se esperaba una dificultad entre 1 y 3. Vuelve a arrancar el juego.")
			  }
  		case 2 => {
  			val estado = cargar("save.dat")
  			estado._2 match {
  			  case 1 => {
  			    val gui = crearGUI(7,9)
  			    bucle(estado._1,estado._2,estado._3, gui)
  			  }
  			  case 2 => {
  			    val gui = crearGUI(11,17)
  			    bucle(estado._1,estado._2,estado._3, gui)
  			  }
  			  case 3 => {
  			    val gui = crearGUI(15,27)
  			    bucle(estado._1,estado._2,estado._3, gui)
  			  }
  			}
  		}
  	}
  }
  
  init()
}