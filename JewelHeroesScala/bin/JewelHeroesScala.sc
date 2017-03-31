object JewelHeroesScala {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
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
  val lst:List[Int] = 1::2::3::4::5::6::7::8::Nil //> lst  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)
  imprimir(lst)                                   //> A	R	N	V	P	M	G	B	res0: Boolean = 
                                                  //| true
}