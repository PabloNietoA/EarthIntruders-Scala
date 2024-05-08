import scala.util.Random

object Main
{
  //dimensiones de la matriz
  val altura: Int = 10
  val anchura: Int = 15

  def generarTablero(n:Int):List[Char] = n match
  {
    case 0 => Nil
    case num => ' '::generarTablero(num-1)
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

  def sublist (i : Int, f : Int, l : List[Char]) : List[Char] = l match
  {
    case Nil => Nil
    case _ if (longitud(l) < f) => throw new Error("Longitud de lista excedida")
    case _ if (f < 0 || i < 0) => throw new Error("Ni el inicio ni final pueden ser negativos")
    case _ if (f < i) => throw new Error("El inicio debe ser menor que el final")

    case _ :: r if (i > 0) => sublist(i-1, f-1, r)
    case p :: r if (i == 0 && f > 0) => p :: sublist(i, f-1, r)
    case _ :: _ if (i == 0 && f == 0) => Nil
  }

  def concat (l1:List[Char], l2:List[Char]): List[Char] = l1 match
  {
    case Nil => l2 match
    {
      case Nil => Nil
      case p :: r => p::concat(Nil, r)
    }
      case p :: r => p::concat(r, l2)
  }

  def reverse (l:List[Char]):List[Char] = l match
  {
    case Nil => Nil
    case p :: r => concat(reverse(r), p::Nil)
    case x => x
  }

  def get(i:Int, l:List[Char]):Char = l match {
    case Nil => throw new Error("Index out of range")
    case p :: _ if (i == 0) => p
    case _ :: r if (i > 0) => get(i-1, r)
    case _ => throw new Error("Negative index")
  }

  def replace(c:Char, i:Int, l:List[Char]):List[Char] = {
    if (i >= longitud(l) || i < 0) throw new Error("Index out of range")
    val lizq = sublist(0, i, l)
    val lder  = sublist(i+1, longitud(l), l)
    return concat(lizq, c::lder)
  }

  def printTablero(t:List[Char]):Unit = {
    t match {
      case Nil => print("\n")
      case p :: r if (longitud(r) % anchura == 0) =>
        print("["+p+"]\n")
        printTablero(r)
      case p :: r =>
        print("["+p+"]")
        printTablero(r)
    }
  }

  def generarAliens(i:Int):List[Char] = {
    val rand = new Random()
    val r = rand.nextInt() % 101
    if (i > 0)
    {
      if (r < 40) return 'A' :: generarAliens(i - 1)
      else if (r < 65) return 'N' :: generarAliens(i - 1)
      else if (r < 80) return 'C' :: generarAliens(i - 1)
      else if (r < 85) return 'D' :: generarAliens(i - 1)
      else if (r < 98) return 'R' :: generarAliens(i - 1)
      else return 'X' :: generarAliens(i - 1)
    }
    else return Nil
  }

  def bajarAliens(t:List[Char]):List[Char] = {
    val avance = sublist(0, longitud(t)-anchura, t)
    val add = generarAliens(anchura)
    return concat(add, avance)
  }

  def transformar(l:List[Char]):List[Char] = {
    transformarAux(anchura*altura-1, borrarTransf(anchura*altura-1, marcarTransfs(l)))
  }
  def transformarAux(i:Int, l:List[Char]):List[Char] = {
    if (i == 0) return l
    else
    {
      if (get(i, l) == 'n') return transformarAux(i-1, replace('N', i, l))
      if (get(i, l) == 'c') return transformarAux(i-1, replace('C', i, l))
      return transformarAux(i-1, l)
    }
  }
  def borrarTransf(i:Int, l:List[Char]):List[Char] = {
    if (i == 0) return l
    else
    {
      if (get(i, l) == 'A')
      {
        if(i % anchura < anchura-1 && get(i+1, l) == 'n')
          return borrarTransf(i-1, replace(' ', i, l))
        if(i % anchura > 0 && get(i-1, l) == 'n')
          return borrarTransf(i-1,replace(' ', i, l))
        if(i < longitud(l)-anchura && get(i+anchura, l) == 'n')
          return borrarTransf(i-1,replace(' ', i, l))
        if(i > anchura && get(i-anchura, l) == 'n')
          return borrarTransf(i-1,replace(' ', i, l))

        if(i % anchura < anchura-1 && get(i+1, l) == 'c')
          return borrarTransf(i-1,replace(' ', i, l))
        if(i % anchura > 0 && get(i-1, l) == 'c')
          return borrarTransf(i-1,replace(' ', i, l))
        if(i < longitud(l)-anchura && get(i+anchura, l) == 'c')
          return borrarTransf(i-1,replace(' ', i, l))
        if(i > anchura && get(i-anchura, l) == 'c')
          return borrarTransf(i-1,replace(' ', i, l))
      }
      if (get(i, l) == ' ')
      {
        val rand = new Random().nextInt() % 101
        if (rand <= 10)
        {
          if (i % anchura < anchura - 1 && get(i + 1, l) == 'X')
            return borrarTransf(i - 1, replace('N', i, l))
          if (i % anchura > 0 && get(i - 1, l) == 'X')
            return borrarTransf(i - 1, replace('N', i, l))
          if (i < longitud(l) - anchura && get(i + anchura, l) == 'X')
            return borrarTransf(i - 1, replace('N', i, l))
          if (i > anchura && get(i - anchura, l) == 'X')
            return borrarTransf(i - 1, replace('N', i, l))
        }
      }
      return borrarTransf(i-1, l)
    }
  }

  def marcarTransfs(l:List[Char]):List[Char] = marcarTransfsAux(anchura*altura, l)
  def marcarTransfsAux(i:Int, l:List[Char]):List[Char] = {
    if (i == 0) return l
    else return marcarTransfsAux(i-1, marcarTransf(i, l))
  }

  def marcarTransf(i:Int, t:List[Char]):List[Char] = {
    if (i > anchura && i < longitud(t)-anchura && i % anchura > 0 && i % anchura < anchura-1)
    {
      if (get(i, t) == 'A')
      {
        if ((get(i + anchura, t) == 'A' || get(i + anchura, t) == 'n') &&
          (get(i - anchura, t) == 'A' || get(i - anchura, t) == 'n') &&
          (get(i+1, t) == 'A' || get(i + 1, t) == 'n') &&
          (get(i-1, t) == 'A' || get(i -1, t) == 'n'))
        {
          return replace ('n', i, t)
        }
      }
      else if (get(i, t) == 'N')
      {
        if ((get(i + anchura, t) == 'A' || get(i + anchura, t) == 'n') &&
          (get(i - anchura, t) == 'A' || get(i - anchura, t) == 'n') &&
          (get(i+1, t) == 'A' || get(i + 1, t) == 'n') &&
          (get(i-1, t) == 'A' || get(i -1, t) == 'n'))
        {
          return replace ('c', i, t)
        }
      }
    }
    return t
  }

  def main (args:Array[String]):Unit =
  {
    val l1 = List('n','b', 'c')
    val l2 = List('c', 'd')
    val t = generarTablero(altura*anchura)
    print("\n")
    val x = bajarAliens(bajarAliens(bajarAliens(bajarAliens(bajarAliens(t)))))
    printTablero(transformar(x))
  }

}