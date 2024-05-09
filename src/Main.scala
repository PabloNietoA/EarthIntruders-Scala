import scala.util.Random, scala.math.ceil

object Main
{
  //dimensiones de la matriz
  val altura: Int = 7
  val anchura: Int = 15



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

  def generarTablero(n:Int):List[Char] = n match
  {
    case 0 => Nil
    case num => ' '::generarTablero(num-1)
  }
  def printTablero(t:List[Char]):Unit = {
    t match {
      case Nil => print("\n")
      case p :: r if (longitud(r) % anchura == 0) =>
        if (p == 'A')
          print("["+Console.GREEN+p+Console.RESET+"]\n")
        else if (p == 'N')
          print("["+"\u001B[38;5;130m"+p+Console.RESET+"]\n")
        else if (p == 'C')
          print("["+Console.BLUE+p+Console.RESET+"]\n")
        else if (p == 'D')
          print("["+"\u001B[38;5;169m"+p+Console.RESET+"]\n")
        else if (p == 'R')
          print("["+Console.YELLOW+p+Console.RESET+"]\n")
        else if (p == 'X')
          print("["+Console.RED+p+Console.RESET+"]\n")
        else
          print("["+p+"]\n")
        printTablero(r)
      case p :: r =>
        if (p == 'A')
          print("["+Console.GREEN+p+Console.RESET+"]")
        else if (p == 'N')
          print("["+"\u001B[38;5;130m"+p+Console.RESET+"]")
        else if (p == 'C')
          print("["+Console.BLUE+p+Console.RESET+"]")
        else if (p == 'D')
          print("["+"\u001B[38;5;169m"+p+Console.RESET+"]")
        else if (p == 'R')
          print("["+Console.YELLOW+p+Console.RESET+"]")
        else if (p == 'X')
          print("["+Console.RED+p+Console.RESET+"]")
        else
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

  def generarPlayer(n:Int):List[Char] = n match {
    case 0 => Nil
    case i if (i-1 == anchura/2) => 'W'::generarPlayer(n-1)
    case _ => ' '::generarPlayer(n-1)
  }
  def getPosPlayer(l:List[Char]):Int = anchura - getPosPlayerAux(anchura, l)
  def getPosPlayerAux(i:Int, l:List[Char]):Int = l match {
    case Nil => throw new Error("No hay W")
    case 'W'::_ => i
    case _::r => getPosPlayerAux(i-1, r)
  }
  def moverPlayer(p:List[Char]):List[Char] = scala.io.StdIn.readLine().toLowerCase() match {
    case "a" =>
      val player = getPosPlayer(p)
      return replace('W',player-1,replace(' ', player, p))
    case "d" =>
      val player = getPosPlayer(p)
      return replace('W',player+1,replace(' ', player, p))
    case _ => return p
  }
  def dibujarPlayer(t:List[Char], p:List[Char]):List[Char] = {
    val posPRel = getPosPlayer(p)
    val iniPRow = longitud(t)-anchura
    val posP = posPRel + iniPRow
    return replace('W', posP, t)
  }

  def perderVida(p:List[Char], t:List[Char], v:Int):Int = {
    val posPRel = getPosPlayer(p)
    val iniPRow = longitud(t)-anchura
    val posP = posPRel + iniPRow
    if (get(posP, t) != ' ') return v-1
    return v
  }

  def generarDefensas(n:Int, contador:Int):List[Char] = n match
  {
    case 0 => Nil
    case _ if (contador >= 3) => ' '::generarDefensas(n-1, 0)
    case _ if (new Random().nextInt() % 101 <= 15) => 'B'::generarDefensas(n-1, contador+1)
    case _ => ' '::generarDefensas(n-1,0)
  }
  def dibujarDefensas(t:List[Char], m:List[Char], i:Int):List[Char] = {
    val iniPRow = longitud(t)-anchura*5
    val pos = i + iniPRow
    if (i == anchura) return t
    else if (get(i, m) == 'B') return dibujarDefensas(replace('B', pos, t), m, i+1)
    else return dibujarDefensas(t, m, i+1)
  }
  def borrarDefensas(t:List[Char], i:Int):List[Char] = {
    val iniPRow = longitud(t)-anchura*4
    val pos = i + iniPRow
    if (i == anchura) return t
    else if (get(pos, t) == 'B') return borrarDefensas(replace(' ', pos, t), i+1)
    else return borrarDefensas(t, i+1)
  }
  def romperDefensas(t:List[Char], m:List[Char], i:Int):List[List[Char]] = {
    val iniPRow = longitud(t)-anchura*5
    val pos = i + iniPRow
    if (i < anchura && get(pos, t) == 'R' && get(i, m) == 'B')
      {
        if (new Random().nextInt() % 101 <= 50)
        {
          val tablero = borrarColumna(i, t)
          val muros = replace(' ', i, m)
          return romperDefensas(tablero, muros, i+1)
        }
        else
        {
          val tableroymuros = borrarFila(pos, t, m)
          val tablero = getList(0, tableroymuros)
          val muros = getList(1, tableroymuros)
          return List(tablero, muros)
        }
      }
    else if (i < anchura) return romperDefensas(t, m, i+1)
    else return List(t, m)
  }

  def borrarFila(i:Int, t:List[Char], m:List[Char]): List[List[Char]] =
  {
    val fila = i / anchura
    print(fila + " " + (longitud(t) / anchura - 4))
    if (fila == longitud(t) / anchura - 5) return List(borrarFilaAux(fila,t), generarTablero(anchura))
    else return List(borrarFilaAux(fila,t), m)

  }
  def borrarFilaAux(i:Int, t:List[Char]):List[Char] =
  {
    val filaIni = i*anchura
    val principio = if (filaIni == 0) Nil else sublist(0, filaIni, t)
    val fin = if (filaIni == longitud(t)-anchura) Nil else sublist(filaIni+anchura, longitud(t), t)
    return concat(principio, concat(generarTablero(anchura), fin))
  }

  def borrarColumna(i:Int, t:List[Char]):List[Char] = borrarColumnaAux(0,i,t)
  def borrarColumnaAux(n:Int, i:Int, tablero:List[Char]): List[Char] ={
    val pos = n*anchura + i
    print(n + " " + pos + " " + i + "\n")
    if(n<altura && get(pos, tablero) == 'W') return borrarColumnaAux(n+1,i,replace('w', pos, tablero))
    else if(n<altura) return borrarColumnaAux(n+1,i,replace(' ', pos, tablero))
    else return tablero
  }

  def getList(i:Int, l:List[List[Char]]):List[Char] = l match {
    case Nil => throw new Error("Index out of range")
    case p :: _ if (i == 0) => p
    case _ :: r if (i > 0) => getList(i-1, r)
    case _ => throw new Error("Negative index")
  }

  def jugar(p:List[Char], t:List[Char], m:List[Char], v:Int):Unit =
  {
    if(v > 0)
    {
      val tableroAux = transformar(bajarAliens(t))
      val tableroymuros = romperDefensas(tableroAux, m, 0)
      val muros = getList(1, tableroymuros)
      val tableroAux2 = dibujarDefensas(borrarDefensas(getList(0, tableroymuros), 0), muros, 0)
      val vida = perderVida(p, tableroAux2, v)
      print("Vidas: "+vida+"\n")
      val tablero = dibujarPlayer(tableroAux2, p)
      printTablero(tablero)
      val player = moverPlayer(p)
      jugar(player, tablero, muros, vida)
    }
    if (v == 0) print("\nGAME OVER\n")
  }

  def main (args:Array[String]):Unit =
  {
    val muros = generarDefensas(anchura, 0)
    val player = generarPlayer(anchura)
    val t = generarTablero(altura*anchura)
    jugar(player, t, muros, 3)
  }

}