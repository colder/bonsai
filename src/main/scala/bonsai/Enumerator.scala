package bonsai

abstract class Enumerator[T, R](grammar: T => Seq[Generator[T, R]]) {
  def getTree(t: T, seed: Int): R
  def iterator(t: T): Iterator[R]
}
