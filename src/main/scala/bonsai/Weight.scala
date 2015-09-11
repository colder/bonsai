package bonsai

case class Weight(w: Int) extends AnyVal with Ordered[Weight] {
  def compare(that: Weight) = this.w - that.w
}

object Weight {
  val default = Weight(1)
  val zero    = Weight(0)
}
