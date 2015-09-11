package bonsai
package enumerators

import scala.collection.mutable.{Queue}

class BottomUpEnumerator[T, R, V](
  val grammar: T => Seq[Generator[T, R]],
  tests: Seq[(Seq[V], V)],
  evaluator: (Vector[Vector[V]], Generator[T, R]) => Option[Vector[V]],
  maxDepth: Int = 10) extends IterativeEnumerator[T, R] with Enumerator[T, R] {

  assert(tests.nonEmpty, "No tests provided")
  assert(tests.map(_._1.size).distinct.size == 1, "Tests with non-uniform input size")

  val inputArity = tests.head._1.size

  val inputVectors = for ( i <- 0 to inputArity-1 ) yield { tests.map(_._1(i)).toVector }
  val outputVector = tests.map(_._2).toVector

  def iterator(t: T): Iterator[R] = {

    new Iterator[R] {
      var depth       = 0
      var values      = Map[T, Vector[Vector[V]]]()
      var witness     = Map[Vector[V], R]()

      val matches     = Queue[R]()

      def getValues(t: T): Vector[Vector[V]] = {
        values.getOrElse(t, {
          // initialize with ground expressions
          val res = getProductions(t).filter(_.subTrees.isEmpty).flatMap { g =>
            evaluator(Vector(), g) match {
              case Some(out) =>
                if (!(witness contains out)) {
                  val expr = g.builder(Nil)
                  witness += out -> expr
                  if (out == outputVector) {
                    matches += expr
                  }
                }
                Some(out)
              case _ =>
                None
            }
          }.toVector

          values += t -> res

          res
        })    
      }

      def expand(): Boolean = {
        var newValues      = values
        var couldExpand    = false;

        depth += 1

        for (t <- values.keys) {
          getNonTerminals(t).foreach { gen =>
            val subValues = gen.subTrees.toVector.map(getValues)
            // cartesian product of subValues
            val subPairs = cartesian(subValues)

            for (pair <- subPairs) {
              evaluator(pair, gen) match {
                case Some(out) =>
                  if (!(witness contains out)) {
                    val expr = gen.builder(pair.map(witness))
                    witness   += out -> expr
                    newValues += t -> (newValues(t) :+ out) 
                    couldExpand = true
                    if (out == outputVector) {
                      matches += expr
                    }
                  }
                case _ =>
              }
            }
          }
        }

        values ++= newValues

        couldExpand
      }

      def next: R = {
        matches.dequeue()
      }

      def hasNext: Boolean = {
        var couldExpand = true
        while(couldExpand && matches.isEmpty && depth < maxDepth) {
          couldExpand = expand()       
        }

        matches.nonEmpty
      }
    
      // init values for T at least
      getValues(t)
    }
  }

  def cartesian[E](ls: Vector[Vector[E]]): Seq[Vector[E]] = {
    val sizes = ls.map(_.size)
    val n = sizes.product

    for(seed <- 0 to n-1) yield {
      var currentSeed = seed;
      (for ((size, i1) <- sizes.zipWithIndex) yield {
        val i2 = currentSeed % size;
        currentSeed = currentSeed / size
        ls(i1)(i2)
      }).toVector
    }
  }

}
