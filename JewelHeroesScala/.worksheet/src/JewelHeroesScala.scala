object JewelHeroesScala {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(69); 
  println("Welcome to the Scala worksheet");$skip(557); 
  
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
  };System.out.println("""imprimir: (tablero: List[Int])Boolean""");$skip(50); 
  val lst:List[Int] = 1::2::3::4::5::6::7::8::Nil;System.out.println("""lst  : List[Int] = """ + $show(lst ));$skip(16); val res$0 = 
  imprimir(lst);System.out.println("""res0: Boolean = """ + $show(res$0))}
}
