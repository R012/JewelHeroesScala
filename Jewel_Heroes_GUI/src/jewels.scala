import javax.swing._
import java.awt.Color
import java.awt.GridLayout
import util.Random
import java.io._
import scala.io.Source

object jewels extends App {
  
    var gui : JFrame = new JFrame()
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
    val root = f.getContentPane
      val sq = root.getComponent(pos)
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
      actualizarGUI(tablero.tail, pos+1, f)
    }
    else {
      f.validate()
      f.repaint()
    }
  }
	
  //Imprime por pantalla el contenido de cualquier iterable
  def printList(args:List[Int]):Unit = {
      args.foreach(println)
  }
  
  //Función que inserta un elemento en una posición dada
	def insertElement(color:Int, pos:Int, table:List[Int]):List[Int] =
		if(table.isEmpty) Nil else if (pos==0) color::table.tail
		else table.head::insertElement(color,pos-1,table.tail)

	//Imprime el table en la consola
	def printTable(table:List[Int], x:Int, y:Int, width:Int, height:Int):Unit = {
	  if(x >= 0 && x < width-1){
	    table(x+y*width) match {
	      case 1 => print("A ")
	      case 2 => print("R ")
	      case 3 => print("N ")
	      case 4 => print("V ")
	      case 5 => print("P ")
	      case 6 => print("M ")
	      case 7 => print("G ")
	      case 8 => print("B ")
	    }
	    printTable(table, x+1, y, width, height)
	  }else if (x==width-1 && y>=0) {
	    table(x+y*width) match {
	      case 1 => print("A")
	      case 2 => print("R")
	      case 3 => print("N")
	      case 4 => print("V")
	      case 5 => print("P")
	      case 6 => print("M")
	      case 7 => print("G")
	      case 8 => print("B")
	    }
	    println()
	    if(y>0)
	      printTable(table, 0, y-1, width, height)
	  }
	}
	
	//Calcula la longitud de una lista
	def listSize(list:List[Int]):Int = {
	  if(!list.isEmpty)
  	  if(list.tail.isEmpty) 1
  	  else  1 + listSize(list.tail)
  	else 0
	}
  
  //Crea un número aleatorio asociado a la dificultad
  def randomNum(difficulty:Int): Int =
  	if(difficulty == 1)(1+Random.nextInt(4))
  	else if(difficulty == 2)(1+Random.nextInt(5))
  	else if(difficulty == 3)(1+Random.nextInt(7))
  	else 0
  
  //Inicializa el tablero con valores aleatorios dependientes de la dificultad
  def initTable (times:Int, difficulty:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initTable (times-1,difficulty):::randomNum(difficulty)::Nil
 		}
	}
  
  //Inicializa un tablero auxiliar rellenándolo con 1
  def initAuxTable (times:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initAuxTable (times-1):::1::Nil
 		}
	}

	/** Busca grupos de jewels en torno a (x, y)
	 * @param lastX Posición X en el último nodo, usada para evitar ciclos.
	 * @param lastY Posición Y en el último nodo, usada para evitar ciclos.
	 * @return Tupla formada por las jewels que forman grupo y los nodos ya visitados
	 */
	def analyzePosition(table:List[Int], visited:List[Int],width:Int,height:Int,x:Int,y:Int,lastX:Int,lastY:Int,value:Int):(List[Int],List[Int]) = {
	  if((x != lastX || y != lastY) && x>=0 && y>=0 && x<width && y < height && x+y*width < width * height && x+y*width >= 0 && visited(x+y*width) == 1 && table(x+y*width) == value){
	    if(lastX == -50 && lastY == -50){ //Caso inicial, lanza las primeras revisiones y devuelve los resultados
	      val right = analyzePosition(table,insertElement(0,x+y*width,visited),width,height,x+1,y,x,y,value)
  	    val left = analyzePosition(table,right._2,width,height,x-1,y,x,y,value)
  	    val up = analyzePosition(table,left._2,width,height,x,y+1,x,y,value)
  	    val down = analyzePosition(table,up._2,width,height,x,y-1,x,y,value)
  	    
  	    if(right._1.size + left._1.size >= 2){
  	      val list = right._1 ::: left._1 ::: up._1 ::: down._1 ::: x :: Nil ::: y :: Nil
  	      return (list, down._2)
  	    }
  	    if(up._1.size + down._1.size >= 2){
  	      val list = right._1 ::: left._1 ::: up._1 ::: down._1 ::: x :: Nil ::: y :: Nil
  	      return (list,down._2)
  	    }
  	    return (Nil,down._2)
	    }
	    if(x - lastX > 0){ //Podamos la rama izquierda
	      val right = analyzePosition(table,insertElement(0,x+y*width,visited),width,height,x+1,y,x,y,value)
  	    val left = Nil
  	    val down = analyzePosition(table,right._2,width,height,x,y+1,x,y,value)
  	    val up = analyzePosition(table,down._2,width,height,x,y-1,x,y,value)
  	    return (right._1 ::: down._1 ::: up._1 ::: x :: Nil ::: y :: Nil,up._2)
	    }
	    if(x - lastX < 0){ //Podamos la rama derecha
	      val right = Nil
  	    val left = analyzePosition(table,insertElement(0,x+y*width,visited),width,height,x-1,y,x,y,value)
  	    val up = analyzePosition(table,left._2,width,height,x,y+1,x,y,value)
  	    val down = analyzePosition(table,up._2,width,height,x,y-1,x,y,value)
  	    return (left._1 ::: up._1 ::: down._1 ::: x :: Nil ::: y :: Nil,down._2)
	    }
	    if(y - lastY > 0){ //Podamos por abajo
	      val right = analyzePosition(table,insertElement(0,x+y*width,visited),width,height,x+1,y,x,y,value)
  	    val left = analyzePosition(table,right._2,width,height,x-1,y,x,y,value)
  	    val up = analyzePosition(table,left._2,width,height,x,y+1,x,y,value)
  	    val down = Nil
  	    return (right._1 ::: left._1 ::: up._1 ::: x :: Nil ::: y :: Nil,up._2)
	    }
	    if(y - lastY < 0){ //Podamos por arriba
	      val right = analyzePosition(table,insertElement(0,x+y*width,visited),width,height,x+1,y,x,y,value)
  	    val left = analyzePosition(table,right._2,width,height,x-1,y,x,y,value)
  	    val up = Nil
  	    val down = analyzePosition(table,left._2,width,height,x,y-1,x,y,value)
  	    return (right._1 ::: left._1 ::: down._1 ::: x :: Nil ::: y :: Nil,down._2)
	    }
	    return (Nil,visited)
	  }else{
	    return (Nil,visited)
	  }
	}
	
	//Intercambia dos posiciones del tablero. Si alguna de ellas no existiese,
	//se devuelve el tablero íntegro
	def swap(table:List[Int], pos1:Int, pos2:Int, elem1:Int):List[Int] = {
	  try{
		  insertElement(elem1,pos2,insertElement(table(pos2),pos1,table))
		} catch {
         case ex: IndexOutOfBoundsException =>{  table }
      }
	}
	
	//Desplaza la columna descendentemente para realizar una eliminación vertical
	def moveColumnDown(table:List[Int], width:Int, height:Int, initialY:Int, x:Int, y:Int, difficulty:Int, jewelsToBeErased:List[Int]):List[Int] = {
	  if(y+1<height){
	    moveColumnDown(swap(table, x+y*width,x+(y+1)*width,table(x+y*width)),width,height,initialY,x,y+1,difficulty,jewelsToBeErased)
	  }else if(!jewelsToBeErased.tail.tail.isEmpty){
	    moveColumnDown(insertElement(randomNum(difficulty), x+y*width, table),width,height,initialY,jewelsToBeErased.tail.tail(1),initialY,difficulty,jewelsToBeErased.tail.tail)
	  }else{
	    insertElement(randomNum(difficulty), x+y*width, table)
	  }
	}
	
	/**
	 * Se crean agrupaciones de jewels verticales que se van a eliminar. En caso de eliminar filas,
	 * se crean grupos verticales de un solo elemento
	 * @param jewelsToBeErased Jewels marcadas para su elimación
	 * @param lastX Posición X en el último nodo, usada para evitar ciclos.
	 * @param lastY Posición Y en el último nodo, usada para evitar ciclos.
	 * @return Grupo de jewels en una dirección cualquiera preparado para realizar una eliminación
	 * vertical.
	 */
	def getJewelGroups(jewelsToBeErased:List[Int], lastX:Int, lastY:Int):List[Int] = {
	  if(!jewelsToBeErased.isEmpty){
	    if(lastX == jewelsToBeErased(0)){
	      return jewelsToBeErased(0) :: Nil ::: jewelsToBeErased(1) :: Nil ::: getJewelGroups(jewelsToBeErased.tail.tail, jewelsToBeErased(0),jewelsToBeErased(1))
	    }else{return Nil}
	  }else{return Nil}
	}
	
	//Elimina los grupos de jewels indicados al tiempo que calcula la puntuación obtenida por ello
	def eraseJewels(table:List[Int], width:Int, height:Int, difficulty:Int, jewelsToBeErased:List[Int], score:Int, sets:Int):Unit = {
	  if(!jewelsToBeErased.isEmpty){
  	  val jewelstoErase = jewelsToBeErased(0) :: Nil ::: jewelsToBeErased(1) :: Nil ::: getJewelGroups(jewelsToBeErased.tail.tail,jewelsToBeErased(0),jewelsToBeErased(1))
  	  val newTable = moveColumnDown(table, width, height, jewelstoErase.reverse(0), jewelstoErase.reverse(1), jewelstoErase.reverse(0),difficulty,jewelstoErase.reverse)
  	  eraseJewels(newTable,width,height,difficulty,jewelsToBeErased.drop(jewelstoErase.size),score+(jewelstoErase.size/2)*25,sets)
	  }else{  //Ya no quedan jewels que eliminar, pasa a la siguiente iteración del bucle
	    gameLoop(table,width,height,difficulty,selection,score,sets+1)
	  }
	}
	
	//Determina si el movimiento realizado permite eliminar jewels
	def manualAnalisis(table:List[Int], difficulty:Int,width:Int, height:Int, x:Int, y:Int, valor:Int, score:Int, sets:Int):Unit = {
		val visited = initAuxTable(width*height)
	  val jewelsToBeErased = analyzePosition(table, visited, width, height, x, y,-50,-50, valor)
		  
		if(jewelsToBeErased._1 == Nil || listSize(jewelsToBeErased._1) < 3) {gameLoop(table,width,height,difficulty,selection,score,sets)}
		else {
			eraseJewels(table,width,height,difficulty,jewelsToBeErased._1,score,sets)
		}
	}
	
	//Función que se encarga de realizar el movimiento indicado y llama a la función de análisis manual
	def swapJewels(table:List[Int], pos1X:Int, pos1Y:Int, direction:Int, width:Int, height:Int, score:Int, sets:Int):Unit = {
		//Determina la dirección y ejecuta el movimiento si no sale del tablero
	  direction match {
		//Arriba
			case 1 => { if(pos1X + (pos1Y+1)*width < width*height && pos1Y+1 < height) {
				manualAnalisis(swap(table, pos1X + pos1Y*width, pos1X + (pos1Y+1)*width, table(pos1X + pos1Y*width)), difficulty,width, height, pos1X, pos1Y+1, table(pos1X + pos1Y*width),score,sets)
			}
			}
		//Abajo
			case 2 => { if(pos1X + (pos1Y-1)*width >= 0 && pos1Y-1 >= 0) {
				manualAnalisis(swap(table, pos1X + pos1Y*width, pos1X + (pos1Y-1)*width, table(pos1X + pos1Y*width)), difficulty,width, height, pos1X, pos1Y-1, table(pos1X + pos1Y*width),score,sets)
			}
			}
		//Izquierda
			case 3 => { if(pos1X-1 + (pos1Y)*width < width*height) { 
			  manualAnalisis(swap(table, pos1X + pos1Y*width, pos1X-1 + (pos1Y)*width, table(pos1X + pos1Y*width)), difficulty,width, height, pos1X-1, pos1Y, table(pos1X + pos1Y*width),score,sets)
			}
			}
		//Derecha
			case 4=> { if(pos1X+1 + (pos1Y)*width >= 0) {
				manualAnalisis(swap(table, pos1X + pos1Y*width, pos1X+1 + (pos1Y)*width, table(pos1X + pos1Y*width)), difficulty,width, height, pos1X+1, pos1Y, table(pos1X + pos1Y*width),score,sets)
			}
			}
		}
	}
	
	/**
	 * Explora todos los movimientos posibles para cada jewel, 
	 * almacenando en todos los casos la longitud y dirección del mejor movimiento existente
	 * @param aux table auxiliar que guarda cuantas jewels se eliminan con su mejor movimiento
	 * @param aux_dir table auxiliar que guarda la dirección hacia la que intercambiar para obtener el valor guardado en "aux" en la misma posición
	 * @return tupla formada por ambos tables auxiliares
	 * 
	 */
	def recursiveAutoAnalysis(table:List[Int], aux:List[Int],directionAux:List[Int], difficulty:Int,width:Int, height:Int, x:Int, y:Int):(List[Int],List[Int]) = {
	  if(y*width+x < height*width) {
  	  val visited = initAuxTable(width*height)
  	  
  	  val erasedAbove = analyzePosition(swap(table, x + y*width, x + (y+1)*width, table(x + y*width)),visited,width,height,x,y+1,-50,-50,table(x+y*width))
  	  val erasedBelow = analyzePosition(swap(table, x + y*width, x + (y-1)*width, table(x + y*width)),visited,width,height,x,y-1,-50,-50,table(x+y*width))
  	  val erasedRight = analyzePosition(swap(table, x + y*width, x+1 + (y)*width, table(x + y*width)),visited,width,height,x+1,y,-50,-50,table(x+y*width))
  	  val erasedLeft = analyzePosition(swap(table, x + y*width, x-1 + (y)*width, table(x + y*width)),visited,width,height,x-1,y,-50,-50,table(x+y*width))
  	  val list = List(erasedAbove._1.size,erasedBelow._1.size,erasedLeft._1.size,erasedRight._1.size)
  	  
  	  if(x < width-1){
  	    recursiveAutoAnalysis(table,insertElement(list.zipWithIndex.maxBy(_._1)._1, x+y*width, aux),insertElement(list.zipWithIndex.maxBy(_._1)._2+1, x+y*width, directionAux),difficulty,width,height,x+1,y)
  	  }else{
  	    recursiveAutoAnalysis(table,insertElement(list.zipWithIndex.maxBy(_._1)._1, x+y*width, aux),insertElement(list.zipWithIndex.maxBy(_._1)._2+1, x+y*width, directionAux),difficulty,width,height,0,y+1)
  	  }
    } else {
	    return (aux,directionAux)
	  }
	}
	
	//Recupera el mejor movimiento posible y lo ejecuta
	def determineBestOption(auxTuple:(List[Int],List[Int]), x:Int, y:Int, bestX:Int, bestY:Int, bestValue:Int, bestDir:Int, table:List[Int], width:Int, height:Int, score:Int, sets:Int):Unit = {
	  if(y*width+x < height*width) {
  	  if(x < width-1){
    	  if(auxTuple._1(x+y*width)>=2 && auxTuple._1(x+y*width) > bestValue){
    	    determineBestOption(auxTuple, x+1, y, x, y, auxTuple._1(x+y*width),auxTuple._2(x+y*width),table,width,height,score,sets)
	      }else{
	        determineBestOption(auxTuple, x+1, y, bestX, bestY, bestValue,bestDir,table,width,height,score,sets)
	      }
  	  }else{
  	    if(auxTuple._1(x+y*width)>=2 && auxTuple._1(x+y*width) > bestValue){
    	    determineBestOption(auxTuple, 0, y+1, x, y, auxTuple._1(x+y*width),auxTuple._2(x+y*width),table,width,height,score,sets)
	      }else{
	        determineBestOption(auxTuple, 0, y+1, bestX, bestY, bestValue,bestDir,table,width,height,score,sets)
	      }
  	  }
	  }else{  //Se ha localizado el mejor
	     swapJewels(table, bestX, bestY, bestDir, width, height,score,sets)
	  }
	}
	
	//Función que prepara y llama el análisis recursivo para el modo automático
	def autoAnalysis(table:List[Int], difficulty:Int,width:Int, height:Int, score:Int, sets:Int):Unit = {
	  val aux = initAuxTable(width*height)
	  val dirAux = initAuxTable(width*height)
	  determineBestOption(recursiveAutoAnalysis(table, aux, dirAux, difficulty, width, height, 0, 0), 0, 0, 0, 0, 0, 4, table, width, height, score, sets)
	}
	
	// Escribe el estado de partida en archivos diferenciados que permitan su fácil recuperación
	def save(table:List[Int], width:Int, height:Int, difficulty:Int, selection:Int, score:Int, sets:Int):Unit = {
	  val fileTable = new PrintWriter(new File("table.txt"))
	  val fileWidth = new PrintWriter(new File("width.txt"))
	  val fileHeight = new PrintWriter(new File("height.txt"))
	  val fileDifficulty = new PrintWriter(new File("difficulty.txt"))
	  val fileSelection = new PrintWriter(new File("selection.txt"))
	  val fileScore = new PrintWriter(new File("score.txt"))
	  val fileSets = new PrintWriter(new File("sets.txt"))
	  fileTable.write(table.mkString(","))
	  fileWidth.write(width.toString())
	  fileHeight.write(height.toString())
	  fileDifficulty.write(difficulty.toString())
	  fileSelection.write(selection.toString())
	  fileScore.write(score.toString())
	  fileSets.write(sets.toString())
	  fileTable.close
	  fileWidth.close
	  fileHeight.close
	  fileDifficulty.close
	  fileSelection.close
	  fileScore.close
	  fileSets.close
	  println("Datos guardados correctamente.")
	  gameLoop(table,width,height,difficulty,selection,score,sets)
	}
	
	// Carga el estado de una partida guardada y lo establece como el actual
	def load():Unit = {
	  val table = Source.fromFile("table.txt").getLines().toList.head.split(",").map(_.trim).toList.map(_.toInt)
	  val width = Source.fromFile("width.txt").getLines().toList.head.toInt
	  val height = Source.fromFile("height.txt").getLines().toList.head.toInt
	  val difficulty = Source.fromFile("difficulty.txt").getLines().toList.head.toInt
	  val selection = Source.fromFile("selection.txt").getLines().toList.head.toInt
	  val score = Source.fromFile("score.txt").getLines().toList.head.toInt
	  val sets = Source.fromFile("sets.txt").getLines().toList.head.toInt
	  println("Partida cargada correctamente.")
	  gameLoop(table,width,height,difficulty,selection,score,sets)
	}
	
	//Bucle de juego normal
	def gameLoop(table:List[Int], width:Int, height:Int, difficulty:Int, selection:Int, score:Int, sets:Int):Unit = {
	  println("\t- Puntuación:\t" + score)
	  println("\t- Numero de conjuntos eliminados:\t" + sets)
	  printTable(table,0,height-1,width,height)
	  actualizarGUI(table,0,gui)
	  println("________________________________")
	  
	  selection match {
	    case 1 => {  //Automatico
	      println("Elige una acción:\n\t1.-Mejor Jugada\n\t 2.-Guardar Partida\n\t3.-Cargar Partida\n\t0.-Salir")
    	  val choice = scala.io.StdIn.readInt()
    	      
    	  choice match {
    	    case 0 => System.exit(1)
    	     case 1 => autoAnalysis(table, difficulty, width, height,score,sets)
    	     case 2 => {println("Guardando partida actual.")
	          save(table, width, height, difficulty, selection, score, sets)}
    	     case 3 => {println("Cargando partida guardada.")
	          load()}case _ => gameLoop(table,width,height,difficulty,selection,score,sets)
    	  }
	    }
	    case 2 => {  //Manual
	      println("Elige una acción:\n\t1.-Desplazar Jewel\n\t2.-Guardar Partida\n\t3.-Cargar Partida\n\t0.-Salir")
	      val choice = scala.io.StdIn.readInt()
	      
	      choice match {
	        case 0 => System.exit(1)
	        case 1 => {  //Intercambio de la jewel a elegir
	          println("Elija la jewel a intercambiar")
	      
    	      println("X:\t")
    	      val x=scala.io.StdIn.readInt()
    	      
    	      println("Y:\t")
    	      val y=scala.io.StdIn.readInt()
    	      
    	      println("Dirección del movimiento:\n\t1.-Arriba\n\t2.-Abajo\n\t3.-Izquierda\n\t4.-Derecha")
    	      val direction=scala.io.StdIn.readInt()
    	      
    	      if(direction < 1 && direction > 4){
    	        println("Dirección inexistente. Por favor, inserte un valor entre 1 y 4.")
    	        gameLoop(table,width,height,difficulty,selection,score,sets)
    	      }
    	      
    	      swapJewels(table, x, y, direction, width, height, score, sets)
	        }
	        case 2 => {
	          println("Guardando partida actual.")
	          save(table, width, height, difficulty, selection, score, sets)
	          
	        }
	        case 3 => {
	          println("Cargando partida guardada.")
	          load()
	        }
	      }
	    }
	    case _ => gameLoop(table,width,height,difficulty,selection,score,sets)
	  }
	  
	}
  
	println("Elija dificultad:\n\t 1.-Fácil\n\t 2.-Medio\n\t 3.-Difícil ")
	val difficulty=scala.io.StdIn.readInt()
	
	println("Elija modo:\n\t 1.-Automático\n\t 2.-Manual ")
	val selection=scala.io.StdIn.readInt()
	
	difficulty match {
	  case 1 => {
	    val table = initTable(7*9,difficulty)
	    gui = crearGUI(7,9)
	    gameLoop(table, 7, 9, difficulty,selection,0,0)
	  }
	  case 2 =>{
	    val table = initTable(11*17,difficulty)
	    gui = crearGUI(11,17)
	    gameLoop(table, 11, 17, difficulty,selection,0,0)
	  }
	  case 3 =>{
	    val table = initTable(15*27,difficulty)
	    gui = crearGUI(15,27)
	    gameLoop(table, 15, 27, difficulty,selection,0,0)
	  }
	  case _ => System.exit(-2)
	}
}