package bonsai

trait TestHelpers {
  object BasicIntGenerators {
    abstract class Label
    case object IntLabel extends Label

    abstract class Expr
    case class Lit(v: Int) extends Expr
    case class Plus(a: Expr, b: Expr) extends Expr
    case class Minus(a: Expr, b: Expr) extends Expr
    case class Times(a: Expr, b: Expr) extends Expr

    val loader: Label => List[Generator[Label, Expr]] = {
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
  }

  object TreeShapesGenerators {
    abstract class Label
    case object TreeLabel extends Label

    abstract class Tree
    case class Node(left: Tree, right: Tree) extends Tree
    case class Leaf() extends Tree

    val loader: Label => List[Generator[Label, Tree]] = {
      case TreeLabel =>
        List(
          Generator(Nil, { _ => Leaf() }),
          Generator(List(TreeLabel, TreeLabel), { case Seq(a,b) => Node(a, b) })
        )
    }
  }
}