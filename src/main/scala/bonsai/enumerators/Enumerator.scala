package bonsai
package enumerators

trait IterativeEnumerator[T, R] {
  def iterator(t: T): Iterator[R]
}

trait RandomAccessEnumerator[T, R] {
  def getTree(t: T, seed: Int): R
}

trait Enumerator[T, R] {

  val grammar: T => Seq[Gen]

  type Gen  = Generator[T, R]

  var generators = Map[T, Seq[Gen]]()
  var builders   = Map[T, Seq[Gen]]()
  var grounds    = Map[T, Seq[R]]()

  def getGenerators(l: T): Seq[Gen] = generators.getOrElse(l, {
    val gens = grammar(l)
    generators += l -> gens
    gens
  })


  def getGrounds(l: T): Seq[R] = grounds.getOrElse(l, {
    getGenerators(l).filter(_.subTrees.isEmpty).map(_.builder(Nil))
  })

  def getBuilders(l: T): Seq[Gen] = builders.getOrElse(l, {
    getGenerators(l).filterNot(_.subTrees.isEmpty)
  })
}
