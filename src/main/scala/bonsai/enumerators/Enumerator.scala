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

  var productions  = Map[T, Seq[Gen]]()
  var nonTerminals = Map[T, Seq[Gen]]()
  var terminals    = Map[T, Seq[R]]()

  def getProductions(l: T): Seq[Gen] = productions.getOrElse(l, {
    val gens = grammar(l)
    productions += l -> gens
    gens
  })


  def getTerminals(l: T): Seq[R] = terminals.getOrElse(l, {
    getProductions(l).filter(_.isTerminal).sortBy(_.weight).map(_.builder(Nil))
  })

  def getNonTerminals(l: T): Seq[Gen] = nonTerminals.getOrElse(l, {
    getProductions(l).filter(_.isNonTerminal).sortBy(_.weight)
  })
}
