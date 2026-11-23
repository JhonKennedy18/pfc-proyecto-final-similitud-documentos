package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

@RunWith(classOf[JUnitRunner])
class SimilitudPropiosTest extends AnyFunSuite {

  val eps = 1e-9

  test("parejas 1: n <= 1 produce vector vacío") {
    assert(parejas(0) == Vector.empty[Pareja])
    assert(parejas(1) == Vector.empty[Pareja])
    assert(parejas(-3) == Vector.empty[Pareja])
  }

  test("parejas 2: n = 2 produce únicamente (0, 1)") {
    assert(parejas(2) == Vector((0, 1)))
  }

  test("parejas 3: n = 4 produce las 6 parejas ordenadas lexicográficamente") {
    assert(parejas(4) == Vector((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("parejas 4: cumple invariante i < j y cardinalidad n(n-1)/2 para n = 20") {
    val ps = parejas(20)
    assert(ps.length == 190)
    assert(ps.forall { case (i, j) => i >= 0 && i < j && j < 20 })
  }

  test("parejas 5: no contiene parejas repetidas") {
    val ps = parejas(12)
    assert(ps.distinct.length == ps.length)
  }

  test("similitudes 1: vector de parejas vacío devuelve vector vacío") {
    val hs = Vector(Set(Vector("a")), Set(Vector("a")))
    assert(similitudes(hs, Vector.empty[Pareja]) == Vector.empty[Medicion])
  }

  test("similitudes 2: conserva exactamente el orden de las parejas recibidas") {
    val hs: Vector[Huella] = Vector(
      Set(Vector("a"), Vector("b")),
      Set(Vector("b"), Vector("c")),
      Set(Vector("a"), Vector("b"))
    )
    val ps = Vector((0, 2), (1, 2), (0, 1))
    val res = similitudes(hs, ps)
    assert(res.map(m => (m._1, m._2)) == ps)
    assert(math.abs(res(0)._3 - 1.0) < eps)
    assert(math.abs(res(1)._3 - (1.0 / 3.0)) < eps)
    assert(math.abs(res(2)._3 - (1.0 / 3.0)) < eps)
  }

  test("similitudes 3: funciona sobre subconjunto de parejas (candidatas)") {
    val hs: Vector[Huella] = Vector(Set(Vector("k1")), Set(Vector("k2")), Set(Vector("k1")))
    assert(similitudes(hs, Vector((0, 2))) == Vector((0, 2, 1.0)))
  }

  test("similitudes 4: maneja huellas vacías dentro del corpus sin fallar") {
    val hs: Vector[Huella] = Vector(Set.empty[Fragmento], Set.empty[Fragmento], Set(Vector("z")))
    assert(similitudes(hs, parejas(3)) == Vector((0, 1, 0.0), (0, 2, 0.0), (1, 2, 0.0)))
  }

  test("similitudes 5: todas las similitudes calculadas están en [0.0, 1.0]") {
    val c = corpusAlAzar(8, 30, 10)
    val hs = huellas(c, 2)
    val res = similitudes(hs, parejas(c.length))
    assert(res.forall { case (_, _, s) => s >= 0.0 && s <= 1.0 })
  }

  test("indiceInvertido 1: vector de huellas vacío produce mapa vacío") {
    assert(indiceInvertido(Vector.empty[Huella]) == Map.empty[Fragmento, Set[Int]])
  }

  test("indiceInvertido 2: documentos con huellas vacías producen mapa vacío") {
    val hs: Vector[Huella] = Vector(Set.empty[Fragmento], Set.empty[Fragmento])
    assert(indiceInvertido(hs).isEmpty)
  }

  test("indiceInvertido 3: fragmento presente en todos los documentos mapea a {0, ..., n-1}") {
    val g = Vector("comun", "todos")
    val hs: Vector[Huella] = Vector(Set(g), Set(g, Vector("a", "b")), Set(g))
    val idx = indiceInvertido(hs)
    assert(idx(g) == Set(0, 1, 2))
    assert(idx(Vector("a", "b")) == Set(1))
  }

  test("indiceInvertido 4: huellas disjuntas asignan conjuntos unitarios a cada fragmento") {
    val hs: Vector[Huella] = Vector(Set(Vector("f0")), Set(Vector("f1")), Set(Vector("f2")))
    val idx = indiceInvertido(hs)
    assert(idx == Map(Vector("f0") -> Set(0), Vector("f1") -> Set(1), Vector("f2") -> Set(2)))
  }

  test("indiceInvertido 5: satisface g in H(d_i) <=> i in I(g) en corpus aleatorio") {
    val c = corpusAlAzar(10, 40, 12)
    val hs = huellas(c, 3)
    val idx = indiceInvertido(hs)
    val condicion = hs.indices.forall { i =>
      hs(i).forall(g => idx.get(g).exists(_.contains(i)))
    }
    assert(condicion)
  }

  test("parejasCandidatas 1: índice vacío devuelve vector vacío") {
    assert(parejasCandidatas(Map.empty[Fragmento, Set[Int]]) == Vector.empty[Pareja])
  }

  test("parejasCandidatas 2: índice donde cada fragmento aparece en un solo doc da vacío") {
    val idx: Indice = Map(Vector("a") -> Set(0), Vector("b") -> Set(1), Vector("c") -> Set(2))
    assert(parejasCandidatas(idx).isEmpty)
  }

  test("parejasCandidatas 3: deduplica parejas que comparten múltiples fragmentos") {
    val idx: Indice = Map(
      Vector("f1") -> Set(0, 2),
      Vector("f2") -> Set(2, 0),
      Vector("f3") -> Set(0, 2)
    )
    assert(parejasCandidatas(idx) == Vector((0, 2)))
  }

  test("parejasCandidatas 4: un fragmento compartido por 4 docs genera las 6 parejas ordenadas") {
    val idx: Indice = Map(Vector("compartido") -> Set(3, 1, 0, 2))
    assert(parejasCandidatas(idx) == Vector((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("parejasCandidatas 5: combina entradas disjuntas y solapadas en orden lexicográfico") {
    val idx: Indice = Map(Vector("x") -> Set(4, 2), Vector("y") -> Set(0, 3, 2))
    assert(parejasCandidatas(idx) == Vector((0, 2), (0, 3), (2, 3), (2, 4)))
  }
}