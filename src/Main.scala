import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime}
import scala.util.Random
import scala.math.ceil
import scalaj.http._

object Main
{
  //dimensiones de la matriz
  val altura: Int = seleccionarDim("altura")
  val anchura: Int = seleccionarDim("anchura")



  //Operaciones de listas



  /**
   * Calcula la longitud de una lista de caracteres
   * @param l lista analizada
   * @return la longitud
   */
  def longitud(l:List[Char]): Int = l match
  {
    case Nil => 0
    case _ :: r => 1 + longitud(r)
  }

  /**
   * Devuelve una sublista de la lista l entre i y f-1 (de caracteres)
   * @param i id del inicio de la sublista incluyendo i
   * @param f id del final de la sublista excluyendo f
   * @param l lista inicial
   * @return la sublista de caracteres
   */
  def sublist (i : Int, f : Int, l : List[Char]) : List[Char] = l match
  {
    case Nil => Nil
    //case _ if (longitud(l) < f) => throw new Error("Longitud de lista excedida")
    //case _ if (f < 0 || i < 0) => throw new Error("Ni el inicio ni final pueden ser negativos")
    case _ if (f < i) => throw new Error("El inicio debe ser menor que el final")

    case _ :: r if (i > 0) => sublist(i-1, f-1, r)
    case p :: r if (i == 0 && f > 0) => p :: sublist(i, f-1, r)
    case _ :: _ if (i == 0 && f == 0) => Nil
  }

  /**
   * Concatena dos listas de caracteres
   * @param l1 primera lista de caracteres
   * @param l2 segunda lista de caracteres
   * @return concatenacion l1:::l2
   */
  def concat (l1:List[Char], l2:List[Char]): List[Char] = l1 match
  {
    case Nil => l2 match
    {
      case Nil => Nil
      case p :: r => p::concat(Nil, r)
    }
      case p :: r => p::concat(r, l2)
  }

  /**
   * da la vuelta a la lista de caracteres
   * @param l lista a voltear
   * @return lista volteada
   */
  def reverse (l:List[Char]):List[Char] = l match
  {
    case Nil => Nil
    case p :: r => concat(reverse(r), p::Nil)
    case x => x
  }

  /**
   * Obtiene un elemento de la lista de caracteres
   * @param i id del elemento
   * @param l lista de caracteres
   * @return valor del elemento
   */
  def get(i:Int, l:List[Char]):Char = l match {
    case Nil => throw new Error("Index out of range")
    case p :: _ if (i == 0) => p
    case _ :: r if (i > 0) => get(i-1, r)
    case _ => throw new Error("Negative index")
  }

  /**
   * reemplaza el valor i por c en la lista de caracteres
   * @param c el caracter a reemplazar
   * @param i id del caracter remplazado
   * @param l lista de caracteres
   * @return lista con caracter cambiado
   */
  def replace(c:Char, i:Int, l:List[Char]):List[Char] = {
    if (i >= longitud(l) || i < 0) throw new Error("Index out of range")
    val lizq = sublist(0, i, l)
    val lder  = sublist(i+1, longitud(l), l)
    return concat(lizq, c::lder)
  }

  /**
   * genera una lista de caracteres de longitud n con ' '
   * @param n numero de elementos de la lista generada
   * @return lista generada
   */
  def generarTablero(n:Int):List[Char] = n match
  {
    case 0 => Nil
    case num => ' '::generarTablero(num-1)
  }

  /**
   * imprime la lista en forma de tablero
   * @param t lista a imprimir
   */
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

  /**
   * toma un elemento de una lista de listas
   * @param i id del elemento
   * @param l lista de listas de caracteres
   * @return lista de caracteres
   */
  def getList(i:Int, l:List[List[Char]]):List[Char] = l match {
    case Nil => throw new Error("Index out of range")
    case p :: _ if (i == 0) => p
    case _ :: r if (i > 0) => getList(i-1, r)
    case _ => throw new Error("Negative index")
  }

  /**
   * toma un elemento de una lista de enteros
   * @param i id del elemento
   * @param l lista de enteros
   * @return entero resultante
   */
  def getInt(i:Int, l:List[Int]):Int = l match {
    case Nil => throw new Error("Index out of range")
    case p :: _ if (i == 0) => p
    case _ :: r if (i > 0) => getInt(i-1, r)
    case _ => throw new Error("Negative index")
  }

  /**
   * develve el elemento string en la posicion i de la lista
   * @param i posicion del elemento
   * @param l lista
   * @return string
   */
  def getString(i:Int, l:List[String]):String = l match {
    case Nil => throw new Error("Index out of range")
    case p :: _ if (i == 0) => p
    case _ :: r if (i > 0) => getString(i-1, r)
    case _ => throw new Error("Negative index")
  }



  //Alienígenas



  /**
   * genera una linea de alienigenas
   * @param i numero de aliens en la linea
   * @return la linea de aliens
   */
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

  /**
   * desplaza la lista una linea, borrando la ultima linea y añade una generada al principio
   * @param t la lista del tablero
   * @return el nuevo tablero
   */
  def bajarAliens(t:List[Char]):List[Char] = {
    val avance = sublist(0, longitud(t)-anchura, t)
    val add = generarAliens(anchura)
    return concat(add, avance)
  }

  /**
   * realiza las transformaciones de los alienigenas
   * @param l lista con el tablero
   * @return tablero transformado
   */
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

  /**
   *
   * @param i finaliza las transformaciones
   * @param l lista del tablero
   * @return tablero transformado
   */
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

  /**
   * marca las transformaciones para el borrado de elementos
   * @param l lista del tablero
   * @return tablero marcado
   */
  def marcarTransfs(l:List[Char]):List[Char] = marcarTransfsAux(anchura*altura, l)
  def marcarTransfsAux(i:Int, l:List[Char]):List[Char] = {
    if (i == 0) return l
    else return marcarTransfsAux(i-1, marcarTransf(i, l))
  }

  /**
   * marca una transformacion en la casilla i si existe
   * @param i posicion de la casilla
   * @param t tablero
   * @return tablero marcado en casilla
   */
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



  //Jugador



  /**
   * genera la lista con la posicion del jugador
   * @param n longitud de la lista
   * @return lista con el player en el centro
   */
  def generarPlayer(n:Int):List[Char] = n match {
    case 0 => Nil
    case i if (i-1 == anchura/2) => 'W'::generarPlayer(n-1)
    case _ => ' '::generarPlayer(n-1)
  }

  /**
   * devuelve la posicion del jugador en el mapa
   * @param l lista del jugador
   * @return posicion del jugador
   */
  def getPosPlayer(l:List[Char]):Int = anchura - getPosPlayerAux(anchura, l)
  def getPosPlayerAux(i:Int, l:List[Char]):Int = l match {
    case Nil => throw new Error("No hay W")
    case 'W'::_ => i
    case _::r => getPosPlayerAux(i-1, r)
  }

  /**
   * mueve al jugador tanto en manual como en automatico
   * @param p lista del jugador
   * @param modo modo de juego
   * @return lista de jugador desplazada
   */
  def moverPlayer(p:List[Char], modo:Char, tablero:List[Char]):List[Char] = modo match {
    case 'm' => scala.io.StdIn.readLine().toLowerCase() match {
      case "a" =>
        val player = getPosPlayer(p)
        if (player>0)return replace('W',player-1,replace(' ', player, p))
        else return p
      case "d" =>
        val player = getPosPlayer(p)
        if(player<anchura-1)return replace('W',player+1,replace(' ', player, p))
        else return p
      case _ => return p
    }
    case 'a' =>
      Thread.sleep(500)
      val player = getPosPlayer(p)
      val izq = longitud(tablero) - anchura*3 - 1 + player
      val der = longitud(tablero) - anchura*3 + 1 + player
      val centro = longitud(tablero) - anchura*3 + player
      if (get(centro, tablero) == ' ' && get(izq, tablero) == ' ' && get(der, tablero) == ' ')
      {
        val rand = new Random().nextInt() % 3
        if (rand == 0 && player > 0) return replace('W', player - 1, replace(' ', player, p))
        else if (rand == 1 && player < anchura - 1) return replace('W', player + 1, replace(' ', player, p))
        else return p
      }
      else if (get(centro, tablero) == ' ') return p
      else if (get(izq, tablero) == ' ') return replace('W',player-1,replace(' ', player, p))
      else if (get(der, tablero) == ' ') return replace('W',player+1,replace(' ', player, p))
      else
      {
        val rand = new Random().nextInt() % 3
        if (rand == 0 && player > 0) return replace('W', player - 1, replace(' ', player, p))
        else if (rand == 1 && player < anchura - 1) return replace('W', player + 1, replace(' ', player, p))
        else return p
      }
  }

  /**
   * dibuja al jugador en el tablero
   * @param t tablero
   * @param p lista del jugador
   * @return tablero con jugador
   */
  def dibujarPlayer(t:List[Char], p:List[Char]):List[Char] = {
    val posPRel = getPosPlayer(p)
    val iniPRow = longitud(t)-anchura
    val posP = posPRel + iniPRow
    return replace('W', posP, t)
  }

  /**
   * resta vida en caso de explosion
   * @param p lista del jugador
   * @param t tablero
   * @param v vida actual
   * @return vida disminuida
   */
  def perderVida(p:List[Char], t:List[Char], v:Int):Int = {
    val posPRel = getPosPlayer(p)
    val iniPRow = longitud(t)-anchura
    val posP = posPRel + iniPRow
    if (get(posP, t) != ' ') return v-1
    return v
  }



  //Muros



  /**
   * genera una lista con los muros
   * @param n tamaño de la lista
   * @param contador contador para que no haya más de 3 (default 0)
   * @return lista con los muros
   */
  def generarDefensas(n:Int, contador:Int):List[Char] = n match
  {
    case 0 => Nil
    case _ if (contador >= 3) => ' '::generarDefensas(n-1, 0)
    case _ if (new Random().nextInt() % 101 <= 15) => 'B'::generarDefensas(n-1, contador+1)
    case _ => ' '::generarDefensas(n-1,0)
  }

  /**
   * dibuja los muros en el tablero
   * @param t tablero
   * @param m lista de muros
   * @param i contador (default 0)
   * @return tablero con muros
   */
  def dibujarDefensas(t:List[Char], m:List[Char], i:Int):List[Char] = {
    val iniPRow = longitud(t)-anchura*5
    val pos = i + iniPRow
    if (i == anchura) return t
    else if (get(i, m) == 'B') return dibujarDefensas(replace('B', pos, t), m, i+1)
    else return dibujarDefensas(t, m, i+1)
  }

  /**
   * borra los muros residuales del tablero
   * @param t tablero
   * @param i contador (default 0)
   * @return tablero sin muros residuales
   */
  def borrarDefensas(t:List[Char], i:Int):List[Char] = {
    val iniPRow = longitud(t)-anchura*4
    val pos = i + iniPRow
    if (i == anchura) return t
    else if (get(pos, t) == 'B') return borrarDefensas(replace(' ', pos, t), i+1)
    else return borrarDefensas(t, i+1)
  }

  /**
   * detecta y activa colisiones especiales con los muros
   * @param t tablero
   * @param m lista de muros
   * @param i contador (default 0)
   * @param v vidas
   * @param p puntuacion
   * @return List(tablero, muros, List(vidas))
   */
  def romperDefensas(t:List[Char], m:List[Char], i:Int, v:Int, p:List[Char]):List[List[Char]] = {
    val iniPRow = longitud(t)-anchura*5
    val pos = i + iniPRow
    if (i < anchura && get(pos, t) == 'R' && get(i, m) == 'B')
      {
        if (new Random().nextInt() % 101 <= 50)
        {
          val tablero = borrarColumna(i, t)
          val muros = replace(' ', i, m)
          val vidas = if (get(i, p) == 'W') v-1 else v
          return romperDefensas(tablero, muros, i+1,vidas, p)
        }
        else
        {
          val tableroymuros = borrarFila(pos, t, m)
          val tablero = getList(0, tableroymuros)
          val muros = getList(1, tableroymuros)
          return List(tablero, muros, (v+48).toChar::Nil)
        }
      }
    else if (i < anchura && get(pos, t) == 'D' && get(i, m) == 'B')
    {
      val tablero = borrarCuadrado(pos, t)
      val vidas = destructorExpVidas(0, i-5, p, v)
      return romperDefensas(tablero, m, i+1, vidas, p)
    }
    else if (i < anchura && get(pos, t) == 'X' && get(i,m) == 'B')
    {
      return romperDefensas(t, replace(' ', i, m), i+1, v, p)
    }
    else if (i < anchura) return romperDefensas(t, m, i+1, v, p)
    else return List(t, m, (v+48).toChar::Nil)
  }



  //Contacto con el suelo



  /**
   * suma puntos y vidas
   * @param i contador (default 0)
   * @param t tablero
   * @param v vidas anteriores
   * @param punt puntuacion anterior
   * @return List(vidas, puntuacion)
   */
  def tocarSuelo(i:Int, t:List[Char], v:Int, punt:Int):List[Int] = {
    val sublista = sublist(longitud(t)-anchura, longitud(t), t)
    if (i < anchura){
      if (get(i, sublista) == 'A') return tocarSuelo(i+1, t, v, punt+5)
      else if (get(i, sublista) == 'N') return tocarSuelo(i+1, t, v, punt+25)
      else if (get(i, sublista) == 'C') return tocarSuelo(i+1, t, v, punt+15)
      else if (get(i, sublista) == 'D') return tocarSuelo(i+1, t, v, punt+5)
      else if (get(i, sublista) == 'R') return tocarSuelo(i+1, t, v, punt+13)
      else if (get(i, sublista) == 'X') return tocarSuelo(i+1, t, v+1, punt+100)
      else return tocarSuelo(i+1, t, v, punt)
    }
    else return List(v, punt)
  }

  /**
   * detecta colisiones especiales con el suelo y las efectua
   * @param t tablero
   * @param i contador (default 0)
   * @param v vidas anteriores
   * @param p lista del jugador
   * @param m lista de muros (redundante)
   * @return List(tablero, List(vidas))
   */
  def explotarSuelo(t:List[Char], i:Int, v:Int, p:List[Char], m:List[Char]):List[List[Char]] = {
    val sublista = sublist(longitud(t)-anchura, longitud(t), t)
    if (i<anchura)
    {
      if (get(i, sublista) == 'D')
      {
        val vidas = destructorExpVidas(0, i-5, p, v)
        return explotarSuelo(borrarCuadrado(i+longitud(t)-anchura,t), i+1, vidas, p, m)
      }
      if (get(i, sublista) == 'R')
      {
        if (new Random().nextInt() % 101 <= 50)
        {
          val tablero = borrarColumna(i, t)
          return explotarSuelo(tablero, i+1, v, p, m)
        }
        else
        {
          val tablero = borrarFila(i+longitud(t)-anchura, t, m)
          return explotarSuelo(getList(0,tablero), i+1, v-1, p, m)
        }
      }
      else return explotarSuelo(t,i+1,v,p,m)
    }
    else return List(t, (v + 48).toChar::Nil)
  }

  /**
   * resta vidas para la explosion del destructor
   * @param i contador (default 0)
   * @param ini posicion desde la que se inicia la explosion - 5
   * @param p lista del jugador
   * @param v vidas anteriores
   * @return vidas posteriores
   */
  def destructorExpVidas(i:Int, ini:Int, p:List[Char], v:Int): Int =
  {
    if (i + ini >= 0 && i + ini < anchura && i + ini < ini + 10)
    {
      if (get(i+ini, p) == 'W') return v-1
    }
    if (i+ini < anchura && i+ini < ini + 10) return destructorExpVidas(i+1, ini, p, v)
    else return v
  }

  /**
   * realiza la explosion de una fila
   * @param i fila de la explosion
   * @param t tablero
   * @param m muros
   * @return List(tablero, muros)
   */
  def borrarFila(i:Int, t:List[Char], m:List[Char]): List[List[Char]] =
  {
    val fila = i / anchura
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

  /**
   * realiza la explosion de una columna
   * @param i posicion de la explosion
   * @param t tablero
   * @return tablero con la explosion realizadas
   */
  def borrarColumna(i:Int, t:List[Char]):List[Char] = borrarColumnaAux(0,i,t)
  def borrarColumnaAux(n:Int, i:Int, tablero:List[Char]): List[Char] ={
    val pos = n*anchura + i
    if(n<altura && get(pos, tablero) == 'W') return borrarColumnaAux(n+1,i,replace('w', pos, tablero))
    else if(n<altura) return borrarColumnaAux(n+1,i,replace(' ', pos, tablero))
    else return tablero
  }

  /**
   * realiza la explosion del destructor
   * @param i posicion de la explosion
   * @param t tablero
   * @return tablero con la explosion realizada
   */
  def borrarCuadrado(i:Int, t:List[Char]): List[Char] =
  {
    val fila = i / anchura
    val columna = i % anchura
    val colIni = if(columna - 5 > 0) columna - 5 else 0
    val colFin = if(columna + 5 < anchura) columna + 5 else anchura - 1
    val filaIni = if(fila - 5 > 0) fila - 5 else 0
    val filaFin = if(fila + 5 < altura) fila + 5 else altura - 1
    borrarSubFila(0, filaIni, filaFin, colIni, colFin, t)
  }

  /**
   * borra una subfila de la explosion cuadrada
   * @param i contador (default 0)
   * @param filaIni fila inicial del cuadrado
   * @param filaFin fila final del cuadrado
   * @param colIni columna inicial del cuadrado
   * @param colFin columna final del cuadrado
   * @param t tablero
   * @return tablero con explosion
   */
  def borrarSubFila(i:Int, filaIni:Int, filaFin:Int, colIni:Int, colFin:Int, t:List[Char]):List[Char] =
  {
    if(i < altura && i>=filaIni && i<=filaFin)
      return borrarSubFila(i+1, filaIni, filaFin, colIni, colFin, borrarSubFilaAux(0, i, colIni, colFin, t))
    else if(i<altura)
      return borrarSubFila(i+1, filaIni, filaFin, colIni, colFin, t)
    else
      return t
  }
  def borrarSubFilaAux(i:Int,fila: Int,colIni:Int, colFin:Int, t:List[Char]):List[Char] =
  {
    val pos = fila * anchura + i
    if (i < anchura && i <= colFin && i >= colIni)
      return borrarSubFilaAux(i+1, fila, colIni, colFin, replace(' ', pos, t))
    else if (i < anchura)
      return borrarSubFilaAux(i+1, fila, colIni, colFin, t)
    else return t
  }



  //Bucle de juego



  def jugar(p:List[Char], t:List[Char], m:List[Char], v:Int, punt:Int, modo:Char):List[String] =
  {
    if(v > 0)
    {
      val expSuelo = explotarSuelo(t, 0, v, p, m)
      val tableroExplotado = getList(0, expSuelo)
      val vidaypunt = tocarSuelo(0,tableroExplotado,get(0,getList(1,expSuelo))-48,punt)
      val vidas = getInt(0, vidaypunt)
      val puntos = getInt(1, vidaypunt)
      val tableroAux = transformar(bajarAliens(tableroExplotado))
      val tableroymuros = romperDefensas(tableroAux, m, 0, vidas, p)
      val muros = getList(1, tableroymuros)
      val tableroAux2 = dibujarDefensas(borrarDefensas(getList(0, tableroymuros), 0), muros, 0)
      val vida = if (get(0,getList(2,tableroymuros)).toInt - 48 != vidas) vidas-1 else perderVida(p, tableroAux2, vidas)
      print("Vidas: "+vida+"\n")
      print("Puntos: "+puntos+"\n")
      val tablero = dibujarPlayer(tableroAux2, p)
      printTablero(tablero)
      val player = moverPlayer(p, modo, t)
      return jugar(player, tablero, muros, vida, puntos, modo)
    }
    else
    {
      print("\nGAME OVER\n")
      print("\nIntroduce tus iniciales: ")
      val nombre = scala.io.StdIn.readLine()

      return List(nombre, punt.toString)
    }
  }

  def seleccionarDim(tipo:String):Int = {
    print("Introduzca la "+tipo+" del tablero: ")
    val respuesta = scala.io.StdIn.readInt()
    tipo match {
      case "altura" if (respuesta>=10) => respuesta
      case "anchura" if (respuesta>=15) => respuesta
      case "altura" => throw new Error("Error: "+tipo+" por debajo de 10")
      case "anchura" => throw new Error("Error: "+tipo+" por debajo de 15")
    }
  }

  def seleccionarModo():Char = {
    print("Introduzca el modo de juego (m/a): ")
    val respuesta = scala.io.StdIn.readChar()
    respuesta match {
      case 'm' => respuesta
      case 'a' => respuesta
      case _ => throw new Error("Error: modo no reconocido")
    }
  }
  def main (args:Array[String]):Unit =
  {
    val ini = LocalDateTime.now()

    val modo = seleccionarModo()
    val muros = generarDefensas(anchura, 0)
    val player = generarPlayer(anchura)
    val t = generarTablero(altura*anchura)
    val nombreypunt =  jugar(player, t, muros, 3, 0, modo)

    val fin = LocalDateTime.now()

    val duracion = Duration.between(ini, fin).getSeconds

    val format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val hora = fin.format(format)

    val nombre = getString(0, nombreypunt)
    val puntos = getString(1, nombreypunt)

    val url = "https://pl3-api-miguel-david.azurewebsites.net/publicar_resultado"
    val response = Http(url).postForm
      .param("nombre", nombre.toString)
      .param("puntuacion", puntos.toString)
      .param("fecha", hora.toString)
      .param("tiempo", duracion.toString)
      .asString
    println(response.body)
  }
}