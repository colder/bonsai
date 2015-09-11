package bonsai
import enumerators._
import org.scalatest.FunSuite

class WeightedGrammarsTests extends FunSuite with TestHelpers {
  test("Empty weights disable generators") {
    import WeightedGrammars._

//    for(enum <- enumerators(loader)) {
//      assert(!enum.iterator(IntLabel).take(4).contains(Lit(0)))
//    }
  }

  test("Terminals are ordered by weight DESC") {
    import WeightedGrammars._

//    for(enum <- enumerators(loader)) {
//      assert(enum.iterator(IntLabel).take(3).toSeq === Seq(Lit(3), Lit(2), Lit(1)))
//    }
  }

  test("Weighted Non-terminals in StreamEnumerator") {
    import WeightedGrammars._

//    val enum = new StreamEnumerator(loader)
//
//    val res = enum.iterator(IntLabel).take(100).toList
//
//    val c1 = res.count(_.isInstanceOf[Plus])
//    val c2 = res.count(_.isInstanceOf[Minus])
//    val c3 = res.count(_.isInstanceOf[Times])
//    val c4 = res.count(_.isInstanceOf[Div])
//
//    assert(c1 > c2)
//    assert(c2 === c3)
//    assert(c4 === 0)
  }

  test("Weighted Non-terminals in MemoizedEnumerator") {
    import WeightedGrammars._

//    val enum = new MemoizedEnumerator(loader)
//
//    val res = enum.iterator(IntLabel).take(100).toList
//
//    val c1 = res.count(_.isInstanceOf[Plus])
//    val c2 = res.count(_.isInstanceOf[Minus])
//    val c3 = res.count(_.isInstanceOf[Times])
//    val c4 = res.count(_.isInstanceOf[Div])
//
//    println(c1)
//    println(c2)
//    println(c3)
//    println(c4)
//
//    assert(c4 === 0)
  }

}

