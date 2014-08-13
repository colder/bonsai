package bonsai

object Bonsai {
  def main(args: Array[String]) {

    abstract class Label
    case object IntLabel extends Label

    abstract class Expr
    case class Lit(v: Int) extends Expr
    case class Plus(a: Expr, b: Expr) extends Expr
    case class Plus3(a: Expr, b: Expr, c: Expr) extends Expr
    case class Minus(a: Expr, b: Expr) extends Expr
    case class Times(a: Expr, b: Expr) extends Expr
    case class UMinus(a: Expr) extends Expr
    case class UPlus(a: Expr) extends Expr

    val genLoader: Label => List[Generator[Label, Expr]] = {
      case IntLabel =>
        List(
          Generator(Nil, { _ => Lit(0) }),
          Generator(Nil, { _ => Lit(1) }),
          Generator(Nil, { _ => Lit(2) }),
          Generator(Nil, { _ => Lit(3) }),
          Generator(List(IntLabel, IntLabel), { case Seq(a,b) => Plus(a, b) }),
          Generator(List(IntLabel, IntLabel), { case Seq(a,b) => Minus(a, b) }),
          Generator(List(IntLabel, IntLabel), { case Seq(a,b) => Times(a, b) })
        )
    }

    val ts = System.currentTimeMillis

    val enum = new Enumerator(genLoader)

    val it = enum.iterator(IntLabel)

    var last: Expr = Lit(0);
    it.take(100000).foreach{ e =>
      //println(e)
      last = e
    }
    println(last)

    println("Finished in "+(System.currentTimeMillis-ts)+"ms")

  }
}
