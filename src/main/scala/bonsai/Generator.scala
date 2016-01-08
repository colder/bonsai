package bonsai

class Generator[T,R](val subTrees: Seq[T], val builder: Seq[R] => R) {
  val arity = subTrees.size

  val isTerminal    = arity == 0
  val isNonTerminal = arity > 0
}
