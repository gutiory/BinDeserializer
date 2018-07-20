import scala.util.Random

val claseA = List("Juan", "Pepe", "Marcos", "Ana", "Julia", "Maria", "Alicia", "Jose", "Mateo")
val claseB = List("Martina", "Claudia", "Pablo", "Samuel", "Dario", "Juan Manuel", "Nuria", "Dolores")

def reparto(nombre:String, lista: List[String]) : (String, List[String]) = {
  nombre -> Random.shuffle(lista.filter(_ != nombre)).take(4)
}

// Elecciones Clase A
val juan = reparto("Juan", claseA)
val pepe = reparto("Pepe", claseA)
val marcos = reparto("Marcos", claseA)
val julia = reparto("Julia", claseA)
val maria = reparto("Maria", claseA)
val alicia = reparto("Alicia", claseA)
val jose = reparto("Jose", claseA)
val mateo = reparto("Mateo", claseA)

// Elecciones Clase B
val martina = reparto("Martina", claseB)
val claudia = reparto("Claudia", claseB)
val pablo = reparto("Pablo", claseB)
val samuel = reparto("Samuel", claseB)
val dario = reparto("Dario", claseB)
val juanManuel = reparto("Juan Manuel", claseB)
val nuria = reparto("Nuria", claseB)
val dolores = reparto("Dolores", claseB)

val m:Map[String, List[String]] = Map()
val map = Map[String, Option[Int]]("one" -> None,
  "two" -> None,
  "three" -> None)

map.filter(_._2.isDefined)


val peticionesClaseA =
  List(juan, pepe, marcos, julia, maria, alicia, jose, mateo)

val peticionesClaseB =
  List(martina, claudia, pablo, samuel, dario, juanManuel, nuria, dolores)

val peticionesTotal = peticionesClaseA ++ peticionesClaseB

def elegirClase(alumno: String, minimo: Int, peticiones: List[String], numClases: Int, clases: Map[Int, List[String]]) : Int = {
  val clasesConMin = clases.filter(_._2.length < minimo)
  if(clasesConMin.size == 0) Random.nextInt(numClases)
  else {
    val clasesCoincidentes = clasesConMin.filter(_._2.contains(peticiones))
    if (clasesCoincidentes.size == 0) Random.nextInt(numClases)
    else clasesCoincidentes.toSeq.sortBy(_._2.length).head._1
  }
  /*
  clases.filter(_._2.length < minimo) match {
    case Map.empty =>
    case clasesConMin => clasesConMin.filter(_._2.contains(peticiones)) match {
      case Map.empty => Random.nextInt(numClases)
      case clasesCoincidentes => clasesCoincidentes.toSeq.sortBy(_._2.length).head._1
    }
  }
  */
}

def reparto(numClases: Int, peticiones: List[(String, List[String])], minimo: Int) : Map[Int, List[String]] = {

  //for ((alumno, peticion) <- peticiones) {
    //val clase = elegirClase(alumno, minimo, peticion, numClases, clases)
    //val listaAlumnos = alumno :: clases(clase)
    //clases.updated(clase, listaAlumnos)
  //}

  //clases

  def loop(peticiones: List[(String, List[String])], clases:Map[Int, List[String]]) : Map[Int, List[String]] = {
    peticiones match {
      case Nil => clases
      case x::xs => {
        val sig = x._1
        val clase = elegirClase(sig, minimo, x._2, numClases, clases)
        println(sig + " ===> " +  clase)
        val listaAlumnos = sig::clases(clase)
        loop(xs, clases.updated(clase, listaAlumnos))
      }
    }
  }
  val clases = Range(0, numClases).map(r => r -> List.empty).toMap
  loop(peticiones, clases)

}

reparto(2, peticionesTotal, 2)