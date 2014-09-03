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

  object ABGenerators {
    abstract class Label
    case object  A extends Label
    case object  B extends Label

    abstract class Tree
    case object A1 extends Tree
    case object A2 extends Tree
    case object A3 extends Tree
    case object B1 extends Tree
    case object B2 extends Tree
    case class BOp1(a: Tree, b: Tree) extends Tree
    case class BOp2(a: Tree) extends Tree
    case class BOp3(b: Tree) extends Tree

    val loader: Label => List[Generator[Label, Tree]] = {
      case A =>
        List(
          Generator(Nil, { _ => A1 }),
          Generator(Nil, { _ => A2 }),
          Generator(Nil, { _ => A3 })
        )
      case B =>
        List(
          Generator(Nil, { _ => B1 }),
          Generator(Nil, { _ => B2 }),
          Generator(List(A, B), { case Seq(a,b) => BOp1(a, b) }),
          Generator(List(A),    { case Seq(a) => BOp2(a) }),
          Generator(List(B),    { case Seq(b) => BOp3(b) })
        )
    }
  }
}
