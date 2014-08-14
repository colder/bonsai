package bonsai
import org.scalatest.FunSuite

class TreesTests extends FunSuite with TestHelpers {
  test("BasicIntOps: sizes") {
    import BasicIntGenerators._

    val enum = new Enumerator(loader)    

    assert(enum.nTreesOf(IntLabel, 0) == 4, "Number of trees 0-3")
    assert(enum.nTreesOf(IntLabel, 1) == 48, "Number of trees of 0-3 applied to +, -, *, depth 1")
    assert(enum.nTreesOf(IntLabel, 2) == 1152, "Number of trees of 0-3 applied to +, -, *, depth 2")
    assert(enum.nTreesOf(IntLabel, 4) == 1161216, "Number of trees of 0-3 applied to +, -, *, depth 4")
  }

  test("BasicIntOps: enum 100000") {
    import BasicIntGenerators._

    val enum = new Enumerator(loader)    
    enum.iterator(IntLabel).take(100000).foreach { e =>

    }
  }

  test("TreeShapes: sizes") {
    import TreeShapesGenerators._

    val enum = new Enumerator(loader)    

    assert(enum.nTreesOf(TreeLabel, 0) == 1,    "Number of shapes of trees with 1 element")
    assert(enum.nTreesOf(TreeLabel, 9) == 4862, "Number of shapes of trees with 10 elements")
  }

  test("TreeShapes: enum 100000") {
    import TreeShapesGenerators._

    val enum = new Enumerator(loader)    
    enum.iterator(TreeLabel).take(100000).foreach { e =>
    }
  }

}
