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
    //???? meter los paneles
    f.setSize(800, 800)
    f.setVisible(true)
  }
  
  /*
    EN: Function that updates the jewels' colours according to the current game status
    ES: Función que actualiza los colores de las jewels de acuerdo al estado del juego
   */
  def actualizarGUI(tablero:List[Int], pos:Int, f:JPanel) {
    if(!tablero.isEmpty){
      f.remove(pos)
      //Colores?? Seleccionar color de alguna forma. Dejo placeholder
      val sq = new JPanel()
      sq.setBackground(Color.red)
      f.add(sq,pos)
      actualizarGUI(tablero.tail, pos+1, f)
    }
    else {
      f.validate()
      f.repaint()
    }
  }
}