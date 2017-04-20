import javax.swing._
import java.awt.GridLayout
import java.awt.Color

object swingExperiments {
  /*
   EN: Function that creates the grid view of the game
   ES: Función que crea la cuadrícula visual del juego
   */
  def crearGUI(ancho:Int, alto:Int) {
    val f = new JFrame("Jewel Heroes")
    val grid = new GridLayout(ancho, alto)
    f.setLayout(grid)
    inicializarGUI(0,ancho*alto,f)
    f.setSize(800, 800)
    f.setVisible(true)
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
}