package bonsai

case class Generator[T,R](kind: T, subTrees: Seq[T], builder: Seq[R] => R) {
  def arity = subTrees.size
}

case class CompiledGenerator[R](subTrees: Seq[Int], builder: Seq[R] => R) {
  val arity = subTrees.size
}
