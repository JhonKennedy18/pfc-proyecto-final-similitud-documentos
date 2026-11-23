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
}