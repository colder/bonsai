package bonsai

case class Generator[T,R](subTrees: Seq[T], builder: Seq[R] => R, weight: Weight = Weight.default) {
  val arity = subTrees.size

  val isTerminal    = arity == 0
  val isNonTerminal = arity > 0
}

case class CompiledGenerator[R](subTrees: Seq[Int], builder: Seq[R] => R, weight: Weight = Weight.default) {
  val arity = subTrees.size

  val isTerminal    = arity == 0
  val isNonTerminal = arity > 0
}
