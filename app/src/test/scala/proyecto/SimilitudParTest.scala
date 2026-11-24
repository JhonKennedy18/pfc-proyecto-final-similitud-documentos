package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._
import SimilitudPar._

/**
 * Pruebas de contrato de la versión paralela: cada una compara contra la
 * versión secuencial, que es la condición que pide el enunciado. No se
 * modifican.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudParTest extends AnyFunSuite {

  val c1: Corpus = Vector(
    ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa")),
    ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra")),
    ("d2", Vector("la", "lluvia", "moja", "el", "parque")))

  val c2: Corpus = Vector(
    ("e0", Vector("la", "funcion", "recursiva", "termina", "cuando", "llega",
      "al", "caso", "base")),
    ("e1", Vector("toda", "funcion", "recursiva", "termina", "cuando", "llega",
      "al", "caso", "base")),
    ("e2", Vector("el", "paralelismo", "divide", "el", "trabajo", "en",
      "tareas", "independientes")),
    ("e3", Vector("el", "paralelismo", "divide", "el", "trabajo", "en",
      "hilos", "independientes")))

  test("huellasPar: da lo mismo que la secuencial") {
    assert(huellasPar(c1, 3) == huellas(c1, 3))
    assert(huellasPar(c2, 3) == huellas(c2, 3))
  }

  test("similitudesPar: da lo mismo que la secuencial, y conserva el orden") {
    val hs = huellas(c2, 3)
    val ps = parejas(c2.length)
    assert(similitudesPar(hs, ps, 1) == similitudes(hs, ps))
    assert(similitudesPar(hs, ps, 100) == similitudes(hs, ps))
  }

  test("fusionar: une los conjuntos del fragmento que está en los dos") {
    val g: Fragmento = Vector("a", "b")
    val h: Fragmento = Vector("c", "d")
    val i1: Indice = Map(g -> Set(0), h -> Set(1))
    val i2: Indice = Map(g -> Set(2))
    assert(fusionar(i1, i2) == Map(g -> Set(0, 2), h -> Set(1)))
    assert(fusionar(i1, Map()) == i1)
  }

  // Pruebas fusionar

  test("Caso 1 - fusionar: une dos índices con claves disjuntas conservando todas las entradas") {
    val i1: Indice = Map(Vector("el", "gato") -> Set(0, 1))
    val i2: Indice = Map(Vector("la", "lluvia") -> Set(2, 3))
    val esperado: Indice = Map(
      Vector("el", "gato") -> Set(0, 1),
      Vector("la", "lluvia") -> Set(2, 3)
    )
    assert(fusionar(i1, i2) == esperado)
  }

  test("Caso 2 - fusionar: cuando comparten un fragmento realiza la unión de sus conjuntos sin sobrescribir") {
    val comun = Vector("caso", "base")
    val i1: Indice = Map(comun -> Set(0, 1), Vector("a", "b") -> Set(0))
    val i2: Indice = Map(comun -> Set(1, 2, 3), Vector("c", "d") -> Set(3))

    val res = fusionar(i1, i2)
    assert(res(comun) == Set(0, 1, 2, 3))
    assert(res(Vector("a", "b")) == Set(0))
    assert(res(Vector("c", "d")) == Set(3))
    assert(res.size == 3)
  }

  test("Caso 3 - fusionar: cumple identidad con índices vacíos a izquierda y derecha") {
    val idx: Indice = Map(Vector("scala", "pura") -> Set(0, 2))
    val vacio: Indice = Map.empty[Fragmento, Set[Int]]

    assert(fusionar(idx, vacio) == idx)
    assert(fusionar(vacio, idx) == idx)
    assert(fusionar(vacio, vacio) == vacio)
  }

  test("Caso 4 - fusionar: es conmutativa (fusionar(i1, i2) == fusionar(i2, i1))") {
    val i1: Indice = Map(
      Vector("f1") -> Set(0, 1),
      Vector("f2") -> Set(2)
    )
    val i2: Indice = Map(
      Vector("f1") -> Set(2, 3),
      Vector("f3") -> Set(1, 3)
    )
    assert(fusionar(i1, i2) == fusionar(i2, i1))
  }

  test("Caso 5 - fusionar: es asociativa ((i1 U i2) U i3 == i1 U (i2 U i3))") {
    val i1: Indice = Map(Vector("x") -> Set(0), Vector("y") -> Set(1))
    val i2: Indice = Map(Vector("x") -> Set(1, 2), Vector("z") -> Set(2))
    val i3: Indice = Map(Vector("y") -> Set(3), Vector("z") -> Set(0, 3))

    assert(fusionar(fusionar(i1, i2), i3) == fusionar(i1, fusionar(i2, i3)))
  }

  test("indiceInvertidoPar: da lo mismo que la secuencial en los ejemplos") {
    assert(indiceInvertidoPar(huellas(c1, 3)) == indiceInvertido(huellas(c1, 3)))
    assert(indiceInvertidoPar(huellas(c2, 3)) == indiceInvertido(huellas(c2, 3)))
  }

  // Casos prueba de indiceInvertidoPar

  test("Caso 1 - indiceInvertidoPar: coincide exactamente con indiceInvertido en el Ejemplo 2") {
    val corpus: Corpus = Vector(
      ("e0", normalizar("la funcion recursiva termina cuando llega al caso base")),
      ("e1", normalizar("toda funcion recursiva termina cuando llega al caso base")),
      ("e2", normalizar("el paralelismo divide el trabajo en tareas independientes")),
      ("e3", normalizar("el paralelismo divide el trabajo en hilos independientes"))
    )
    val hs = huellas(corpus, 3)

    assert(indiceInvertidoPar(hs) == indiceInvertido(hs))
  }

  test("Caso 2 - indiceInvertidoPar: corrige exactamente el desplazamiento cuando el mismo fragmento está en ambos extremos") {
    val comun = Vector("fragmento", "compartido")
    val hs: Vector[Huella] = Vector(
      Set(comun, Vector("doc", "cero")),   // índice 0 (mitad izquierda)
      Set(Vector("doc", "uno")),           // índice 1 (mitad izquierda)
      Set(Vector("doc", "dos")),           // índice 2 (mitad derecha, local 0 -> +2 = 2)
      Set(comun, Vector("doc", "tres")),   // índice 3 (mitad derecha, local 1 -> +2 = 3)
      Set(comun, Vector("doc", "cuatro"))  // índice 4 (mitad derecha, local 2 -> +2 = 4)
    )
    val idxPar = indiceInvertidoPar(hs)

    assert(idxPar(comun) == Set(0, 3, 4))
    assert(idxPar(Vector("doc", "dos")) == Set(2))
    assert(idxPar == indiceInvertido(hs))
  }

  test("Caso 3 - indiceInvertidoPar: vector vacío y de un solo elemento alcanzan el caso base correctamente") {
    val vacio: Vector[Huella] = Vector.empty[Huella]
    assert(indiceInvertidoPar(vacio) == Map.empty[Fragmento, Set[Int]])

    val uno: Vector[Huella] = Vector(Set(Vector("unico", "elemento")))
    assert(indiceInvertidoPar(uno) == indiceInvertido(uno))
  }

  test("Caso 4 - indiceInvertidoPar: maneja corpus de tamaño impar donde las mitades tienen distinto tamaño") {
    val corpusImpar = corpusAlAzar(n = 9, t = 14, v = 5)
    val hs = huellas(corpusImpar, 3)

    assert(indiceInvertidoPar(hs) == indiceInvertido(hs))
  }

  test("Caso 5 - indiceInvertidoPar: equivalencia exacta en corpus grande con copias parciales") {
    val corpus = corpusConCopias(n = 30, t = 25, v = 15, c = 5, f = 0.7)
    val hs = huellas(corpus, 3)

    assert(indiceInvertidoPar(hs) == indiceInvertido(hs))
  }

  test("detectar: las cuatro versiones coinciden") {
    val directo = detectarDirecto(c2, 3, 0.3, 5)
    assert(detectarConIndice(c2, 3, 0.3, 5) == directo)
    assert(detectarDirectoPar(c2, 3, 0.3, 5, 2) == directo)
    assert(detectarConIndicePar(c2, 3, 0.3, 5, 2) == directo)
  }
}
