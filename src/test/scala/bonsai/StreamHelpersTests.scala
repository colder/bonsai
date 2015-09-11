package bonsai
import enumerators._
import org.scalatest.FunSuite

class StreamHelpersTests extends FunSuite with TestHelpers {

  import helpers.StreamHelpers._

  test("Splitting streams by 0") {
    val s = Stream.from(1)

    val ss = split(s, 0)

    assert(ss.isEmpty)
  }

  test("Splitting streams by 1") {
    val s = Stream.from(1)

    val Seq(s1) = split(s, 1)

    assert(s1.take(10).toList == s.take(10))
  }

  test("Splitting streams by 2") {
    val s = Stream.from(1)

    val Seq(s1, s2) = split(s, 2)

    assert(s1.take(4).toList === List(1, 3, 5, 7))
    assert(s2.take(4).toList === List(2, 4, 6, 8))
  }

  test("Splitting streams by 3") {
    val s = Stream.from(1)

    val Seq(s1, s2, s3) = split(s, 3)

    assert(s1.take(4).toList === List(1, 4, 7, 10))
    assert(s2.take(4).toList === List(2, 5, 8, 11))
    assert(s3.take(4).toList === List(3, 6, 9, 12))
  }

  test("Splitting finite streams by 3") {
    val s = (1 to 3).toStream

    val Seq(s1, s2, s3, s4) = split(s, 4)

    assert(s1.toList === List(1))
    assert(s2.toList === List(2))
    assert(s3.toList === List(3))
    assert(s4.toList === Nil)
  }
}

