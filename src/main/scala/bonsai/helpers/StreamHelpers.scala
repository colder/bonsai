package bonsai
package helpers

object StreamHelpers {
  // Stream helpers
  def split[T](stream: Stream[T], by: Int): Seq[Stream[T]] = {
    if (by <= 0) {
      Nil
    } else if (by == 1) {
      Seq(stream)
    } else {
      for (i <- 0 until by) yield {
        every(stream, i, by)
      }
    }
  }

  def every[T](stream: Stream[T], skip: Int, step: Int): Stream[T] = {
    val rest = stream.drop(step)

    val part = stream.drop(skip)
    if (part.isEmpty) {
      part
    } else {
      part.head #:: every(rest, skip, step)
    }
  }

  def interleave[T](streams: Seq[Stream[T]]): Stream[T] = {
    var ss = streams
    while(!ss.isEmpty && ss.head.isEmpty) {
      ss = ss.tail
    }
    if(ss.isEmpty) return Stream.empty
    if(ss.size == 1) return ss(0)
    Stream.cons(ss.head.head, interleave(ss.tail :+ ss.head.tail))
  }

  def naryProduct[T](streams : Seq[Stream[T]]) : Stream[List[T]] = {
    val dimensions = streams.size
    val vectorizedStreams = streams.map(new VectorizedStream(_))

    if(dimensions == 0)
      return Stream.cons(Nil, Stream.empty)

    if(streams.exists(_.isEmpty))
      return Stream.empty

    val indices = if(streams.forall(_.hasDefiniteSize)) {
      val max = streams.map(_.size).max
      diagCount(dimensions).take(max)
    } else {
      diagCount(dimensions)
    }

    var allReached : Boolean = false
    val bounds : Array[Int] = Array.fill(dimensions)(Int.MaxValue)

    indices.takeWhile(_ => !allReached).flatMap { indexList =>
      var d = 0
      var continue = true
      var is = indexList
      var ss = vectorizedStreams.toList

      if(indexList.sum >= bounds.max) {
        allReached = true
      }

      var tuple : List[T] = Nil

      while(continue && d < dimensions) {
        var i = is.head
        if(i > bounds(d)) {
          continue = false
        } else try {
          // TODO can we speed up by caching the random access into
          // the stream in an indexedSeq? After all, `i` increases
          // slowly.
          tuple = (ss.head)(i) :: tuple
          is = is.tail
          ss = ss.tail
          d += 1
        } catch {
          case e : IndexOutOfBoundsException =>
            bounds(d) = i - 1
            continue = false
        }
      }
      if(continue) Some(tuple.reverse) else None
    }
  }

  def diagCount(dim : Int) : Stream[List[Int]] = diag0(dim, 0)
  def diag0(dim : Int, nextSum : Int) : Stream[List[Int]] = summingTo(nextSum, dim).append(diag0(dim, nextSum + 1))

  def summingTo(sum : Int, n : Int) : Stream[List[Int]] = {
    // assert(sum >= 0)
    if(sum < 0) {
      Stream.empty
    } else if(n == 1) {
      Stream.cons(sum :: Nil, Stream.empty) 
    } else {
      (0 to sum).toStream.flatMap(fst => summingTo(sum - fst, n - 1).map(fst :: _))
    }
  }

  class VectorizedStream[T](initial : Stream[T]) {
    private def mkException(i : Int) = new IndexOutOfBoundsException("Can't access VectorizedStream at : " + i)
    private def streamHeadIndex : Int = indexed.size
    private var stream  : Stream[T] = initial
    private var indexed : Vector[T] = Vector.empty

    def apply(index : Int) : T = {
      if(index < streamHeadIndex) {
        indexed(index)
      } else {
        val diff = index - streamHeadIndex // diff >= 0
        var i = 0
        while(i < diff) {
          if(stream.isEmpty) throw mkException(index)
          indexed = indexed :+ stream.head
          stream  = stream.tail
          i += 1
        }
        // The trick is *not* to read past the desired element. Leave it in the
        // stream, or it will force the *following* one...
        stream.headOption.getOrElse { throw mkException(index) }
      }
    }
  }
}
