package bonsai
package enumerators

class StreamEnumerator[T, R](val grammar: T => Seq[Generator[T, R]]) extends IterativeEnumerator[T, R] with RandomAccessEnumerator[T, R] with Enumerator[T, R] {

  import helpers.StreamHelpers._

  var streams = Map[(T, Int), Stream[R]]()

  def getStream(l: T, depth: Int): Stream[R] = streams.getOrElse((l,depth), {
    val r = if (depth == 0) {
      getTerminals(l).toStream
    } else {
      interleave(getNonTerminals(l).map { b =>
        summingTo(depth-1, b.arity).flatMap { sum =>
          naryProduct((b.subTrees zip sum).map{ case (st, sd) => getStream(st, sd) }).map { case ss =>
            b.builder(ss)
          }
        }
      })
    }

    streams += ((l, depth) -> r)
    r
  })

  def fullStream(t: T, depth: Int = 0): Stream[R] = getStream(t, depth).append { fullStream(t, depth+1) }

  def getTree(l: T, absSeed: Int): R = {
    fullStream(l)(absSeed)
  }

  def iterator(t: T): Iterator[R] = {
    fullStream(t).iterator
  }

}
