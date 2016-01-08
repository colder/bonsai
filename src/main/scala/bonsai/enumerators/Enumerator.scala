package bonsai
package enumerators

trait IterativeEnumerator[T, R] {
  def iterator(t: T): Iterator[R]
}

trait RandomAccessEnumerator[T, R] {
  def getTree(t: T, seed: Int): R
}

trait Enumerator[T, R, G <: Generator[T, R]] {

  val grammar: T => Seq[G]

  var productions  = Map[T, Seq[G]]()
  var nonTerminals = Map[T, Seq[G]]()
  var terminals    = Map[T, Seq[R]]()

  def getProductions(l: T): Seq[G] = productions.getOrElse(l, {
    val gens = grammar(l)
    productions += l -> gens
    gens
  })


  def getTerminals(l: T): Seq[R] = terminals.getOrElse(l, {
    getProductions(l).filter(_.isTerminal).map(_.builder(Nil))
  })

  def getNonTerminals(l: T): Seq[G] = nonTerminals.getOrElse(l, {
    getProductions(l).filter(_.isNonTerminal)
  })
}
