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
}