object Main {
  //dimensiones de la matriz
  val altura: Int = pedirValor("altura")
  val anchura: Int = pedirValor("anchura")

  def generarTablero(n:Int):List[Char] = n match
  {
    case 0 => Nil
    case num => '.'::generarTablero(num-1)
  }

  def listaVacia(l:List[Char]): Boolean = l match
  {
    case Nil => true
    case _ :: _ => false
  }

  def longitud(l:List[Char]): Int = l match
  {
    case Nil => 0
    case _ :: r => 1 + longitud(r)
  }

  def subLista (i : Int, f : Int, l : List[Char]) : List[Char] = l match
  {
    case Nil => Nil
    case _ if (longitud(l) < f) => throw new Error("Longitud de lista excedida")
    case _ if (f < 0 || i < 0) => throw new Error("Ni el inicio ni final pueden ser negativos")
    case _ if (f > i) => throw new Error("El inicio debe ser menor que el final")

    case _ :: r if (i > 0) => subLista(i-1, f-1, r)
    case p :: r if (i == 0 && f > 0) => p :: subLista(i, f-1, r)
    case _ :: _ if (i == 0 && f == 0) => Nil
  }
}