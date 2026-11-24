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

  test("ranking 1: filtra estrictamente con >= umbral (incluye borde exacto)") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.5), (0, 2, 0.499), (1, 2, 0.8))
    assert(ranking(ms, 0.5, 10) == Vector((1, 2, 0.8), (0, 1, 0.5)))
  }

  test("ranking 2: rompe empates determinísticamente por (i, j) ascendente") {
    val ms: Vector[Medicion] = Vector((2, 3, 0.6), (0, 2, 0.6), (0, 1, 0.6), (1, 2, 0.6))
    val expected = Vector((0, 1, 0.6), (0, 2, 0.6), (1, 2, 0.6), (2, 3, 0.6))
    assert(ranking(ms, 0.5, 10) == expected)
    assert(ranking(ms.reverse, 0.5, 10) == expected)
  }

  test("ranking 3: trunca correctamente cuando hay más elementos que p") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.9), (0, 2, 0.8), (0, 3, 0.7), (1, 2, 0.6))
    assert(ranking(ms, 0.1, 2) == Vector((0, 1, 0.9), (0, 2, 0.8)))
  }

  test("ranking 4: devuelve vacío si nadie alcanza el umbral o si p <= 0") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.2), (1, 2, 0.3))
    assert(ranking(ms, 0.5, 5).isEmpty)
    assert(ranking(ms, 0.0, 0).isEmpty)
  }

  test("ranking 5: entrada vacía devuelve vector vacío") {
    assert(ranking(Vector.empty[Medicion], 0.0, 5).isEmpty)
  }






  // ---------------------------------------------------------------------------
  // Pruebas propias de detectarDirecto y detectarConIndice
  // ---------------------------------------------------------------------------

  def docJ(id: String, texto: String): Documento =
    (id, texto.split(" ").toVector)

  def cercaJ(a: Double, b: Double): Boolean = math.abs(a - b) < eps

  def igualJ(r: Vector[Medicion], esperado: Vector[Medicion]): Boolean =
    r.length == esperado.length &&
      r.zip(esperado).forall { case ((i, j, s), (ei, ej, es)) =>
        i == ei && j == ej && cercaJ(s, es)
      }

  // Con k = 3: (0,3) = 1 porque el 3 copia al 0; (0,1) = (1,3) = 1/2;
  // (1,2) = 1/9; el resto da 0.
  val dAJ: Corpus = Vector(
    docJ("a0", "el gato negro duerme en casa"),
    docJ("a1", "el gato negro duerme en el patio"),
    docJ("a2", "un perro blanco juega en el patio"),
    docJ("a3", "el gato negro duerme en casa")
  )

  // Con k = 1 (huella = conjunto de palabras): (0,1) = (1,2) = 1/2,
  // (0,2) = 1/5 y el documento 3 no comparte nada.
  val dBJ: Corpus = Vector(
    docJ("b0", "a b c"),
    docJ("b1", "b c d"),
    docJ("b2", "c d e"),
    docJ("b3", "x")
  )

  // Con k = 2: "a a a a a" tiene un solo fragmento (aa) aunque se repita.
  // (0,1) = 1/2 y las demás parejas dan 0.
  val dCJ: Corpus = Vector(
    docJ("c0", "a a a a a"),
    docJ("c1", "a a b"),
    docJ("c2", "b b b")
  )

  // Con k = 2: cadena; solo las parejas adyacentes comparten un fragmento,
  // y cada una da 1/3. El documento 4 queda aislado.
  val iAJ: Corpus = Vector(
    docJ("i0", "a b c"),
    docJ("i1", "b c d"),
    docJ("i2", "c d e"),
    docJ("i3", "d e f"),
    docJ("i4", "z z z")
  )

  // Con k = 2: el fragmento "x y" está en los cuatro documentos y cada uno
  // tiene otro propio; las seis parejas dan 1/3.
  val iBJ: Corpus = Vector(
    docJ("h0", "x y a"),
    docJ("h1", "x y b"),
    docJ("h2", "x y c"),
    docJ("h3", "x y d")
  )

  // Ninguna pareja comparte fragmentos.
  val iCJ: Corpus = Vector(
    docJ("n0", "a b"),
    docJ("n1", "c d"),
    docJ("n2", "e f")
  )

  // Con k = 2: (0,1) = 3/5; con k = 3: (0,1) = 1/2. El documento 2 no
  // comparte nada.
  val iEJ: Corpus = Vector(
    docJ("m0", "a b c d e"),
    docJ("m1", "a b c d f"),
    docJ("m2", "q r s t u")
  )

  test("detectarDirecto 1: corpus vacío o de un documento no da parejas") {
    assert(detectarDirecto(Vector.empty[Documento], 1, 0.1, 5).isEmpty)
    assert(detectarDirecto(Vector(docJ("u", "a b c d")), 1, 0.0, 5).isEmpty)
  }

  test("detectarDirecto 2: una copia exacta encabeza y los empates 1/2 se ordenan por (i, j)") {
    val esperado: Vector[Medicion] = Vector(
      (0, 3, 1.0), (0, 1, 1.0 / 2.0), (1, 3, 1.0 / 2.0), (1, 2, 1.0 / 9.0))
    assert(igualJ(detectarDirecto(dAJ, 3, 0.1, 10), esperado))
    assert(igualJ(detectarDirecto(dAJ, 3, 0.1, 2), esperado.take(2)))
    assert(igualJ(detectarDirecto(dAJ, 3, 0.9, 10), Vector((0, 3, 1.0))))
  }

  test("detectarDirecto 3: con k = 1 el umbral 0.5 es inclusivo y 0.51 lo excluye") {
    assert(igualJ(detectarDirecto(dBJ, 1, 0.5, 10),
      Vector((0, 1, 0.5), (1, 2, 0.5))))
    assert(igualJ(detectarDirecto(dBJ, 1, 0.2, 10),
      Vector((0, 1, 0.5), (1, 2, 0.5), (0, 2, 0.2))))
    assert(detectarDirecto(dBJ, 1, 0.51, 10).isEmpty)
  }

  test("detectarDirecto 4: un fragmento repetido dentro de un documento cuenta una vez") {
    assert(igualJ(detectarDirecto(dCJ, 2, 0.5, 10), Vector((0, 1, 0.5))))
  }

  test("detectarDirecto 5: con umbral 0 incluye las parejas de similitud cero") {
    assert(igualJ(detectarDirecto(dBJ, 1, 0.0, 10), Vector(
      (0, 1, 0.5), (1, 2, 0.5), (0, 2, 0.2),
      (0, 3, 0.0), (1, 3, 0.0), (2, 3, 0.0))))
    assert(igualJ(detectarDirecto(iCJ, 2, 0.0, 10),
      Vector((0, 1, 0.0), (0, 2, 0.0), (1, 2, 0.0))))
  }

  test("detectarDirecto 6: con corpus al azar cumple sus invariantes") {
    (1 to 10).foreach { _ =>
      val c = corpusConCopias(9, 20, 4, 3, 0.7)
      val hs = huellas(c, 3)
      val r = detectarDirecto(c, 3, 0.15, 6)
      assert(r.length <= 6)
      assert(r.forall { case (i, j, s) =>
        i < j && j < c.length && s >= 0.15 && s == jaccard(hs(i), hs(j))
      })
      assert(r.map(_._3) == r.map(_._3).sorted.reverse)
    }
  }

  test("detectarConIndice 1: corpus vacío o de un documento no da parejas") {
    assert(detectarConIndice(Vector.empty[Documento], 2, 0.1, 5).isEmpty)
    assert(detectarConIndice(Vector(docJ("u", "a b c d")), 2, 0.0, 5).isEmpty)
  }

  test("detectarConIndice 2: en una cadena solo se miden las parejas adyacentes") {
    val esperado: Vector[Medicion] = Vector(
      (0, 1, 1.0 / 3.0), (1, 2, 1.0 / 3.0), (2, 3, 1.0 / 3.0))
    assert(igualJ(detectarConIndice(iAJ, 2, 0.3, 10), esperado))
    assert(igualJ(detectarConIndice(iAJ, 2, 0.0, 10), esperado))
  }

  test("detectarConIndice 3: un fragmento común a cuatro documentos y truncado a p") {
    assert(igualJ(detectarConIndice(iBJ, 2, 0.3, 4), Vector(
      (0, 1, 1.0 / 3.0), (0, 2, 1.0 / 3.0), (0, 3, 1.0 / 3.0),
      (1, 2, 1.0 / 3.0))))
    assert(detectarConIndice(iBJ, 2, 0.3, 10).length == 6)
    assert(detectarConIndice(iBJ, 2, 0.3, 0).isEmpty)
  }

  test("detectarConIndice 4: sin fragmentos compartidos no devuelve nada ni con umbral 0") {
    assert(detectarConIndice(iCJ, 2, 0.0, 10).isEmpty)
    assert(detectarConIndice(iCJ, 2, 0.5, 10).isEmpty)
  }

  test("detectarConIndice 5: una pareja con varios fragmentos comunes sale una vez, con k = 2 y k = 3") {
    assert(igualJ(detectarConIndice(iEJ, 2, 0.5, 10),
      Vector((0, 1, 3.0 / 5.0))))
    assert(igualJ(detectarConIndice(iEJ, 3, 0.5, 10),
      Vector((0, 1, 0.5))))
    assert(igualJ(detectarConIndice(iEJ, 3, 0.0, 10),
      Vector((0, 1, 0.5))))
  }

  test("detectarConIndice 6: con corpus al azar solo mide parejas que comparten fragmento") {
    (1 to 10).foreach { _ =>
      val c = corpusConCopias(9, 20, 4, 3, 0.7)
      val hs = huellas(c, 3)
      val r = detectarConIndice(c, 3, 0.0, 100)
      assert(r.forall { case (i, j, s) =>
        i < j && (hs(i) intersect hs(j)).nonEmpty && s > 0.0
      })
      assert(r.map(_._3) == r.map(_._3).sorted.reverse)
    }
  }

  test("detectarDirecto y detectarConIndice 7: coinciden con umbral positivo en corpus al azar") {
    (1 to 20).foreach { _ =>
      val c = corpusConCopias(10, 25, 5, 4, 0.6)
      assert(detectarDirecto(c, 2, 0.1, 8) == detectarConIndice(c, 2, 0.1, 8))
      assert(detectarDirecto(c, 3, 0.05, 100) ==
        detectarConIndice(c, 3, 0.05, 100))
    }
  }
}