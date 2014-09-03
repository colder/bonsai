package bonsai
import enumerators._
import org.scalatest.FunSuite

class ExhaustivnessTests extends FunSuite with TestHelpers {

  test("No Duplicates: {A(3), B(2)} on operations") {
    import ABGenerators._

    var seen = Set[Tree]();
    val enum = new MemoizedEnumerator(loader)

    enum.iterator(B).take(1000).foreach { e =>
      if (seen contains e) {
        fail(" Expression "+e+" already seen!")
      }
      seen += e
    }
  }

  test("Random Find: {A(3), B(2)} on operations") {
    import ABGenerators._

    var seen = Set[Tree]();
    val enum = new MemoizedEnumerator(loader)

    assert(enum.iterator(B).take(1000).find(_ == BOp1(A3, B2)).isDefined)
  }
}
