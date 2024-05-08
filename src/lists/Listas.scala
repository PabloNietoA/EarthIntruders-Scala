package lists {
  class Listas {
    def generarArray(i: Int, j: Int): List[List[Char]] = {
      if(i > 0) return generarLista(j)::generarArray(i-1, j)
      else return Nil
    }
    def generarLista(i:Int):List[Char] = {
      if (i > 0) return '.'::generarLista(i-1)
      else return Nil
    }
    /*def verMat(i:Int, j:Int):Char = {

    }
    def verLista(i:Int, l:List[Char]):Char = {
      if (i > 0){
        return
      }
    }*/
  }
}
