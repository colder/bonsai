package bonsai
package enumerators

import scala.collection.mutable.{ArrayBuffer, BitSet, Map => MutableMap, HashMap => MutableHashMap}

class MemoizedEnumerator[T, R, G <: Generator[T, R]](val grammar: T => Seq[G]) extends IterativeEnumerator[T, R] with RandomAccessEnumerator[T, R] with Enumerator[T, R, G] {

  type CGen = Generator[Int, R]

  // Maximum distinct labels
  val MAX_LABELS = 1024;

  // Maximum consecutive expands without finding new trees
  val MAX_EXPANDS = 5;

  // Compile labels for more efficient processing
  private[this] var nextLabelId = -1;
  private[this] var labelsIds = Map[T, Int]()
  private[this] var idsLabels = Map[Int, T]()
  val cTerminals:  ArrayBuffer[ArrayBuffer[R]] = new ArrayBuffer()
  val cNonTerminals: ArrayBuffer[Seq[CGen]]    = new ArrayBuffer()

  def labelId(l: T): Int = {
    labelsIds.getOrElse(l, {
      nextLabelId += 1
      if (nextLabelId >= MAX_LABELS) {
        throw new BonsaiException("Exceeded the number of labels available")
      }

      cTerminals += new ArrayBuffer[R]()
      cNonTerminals += Nil

      labelsIds  += l -> nextLabelId
      idsLabels += nextLabelId -> l

      nextLabelId
    })
  }

  // Compiled generators (terminals + nonTerminals) for each compiled label
  val isGenInit: BitSet = new BitSet()

  def fetchAndCompileGenerators(l: Int): Unit = {
    val t = idsLabels(l)

    //println("Fetching grammar for "+t)

    isGenInit += l
    cTerminals(l)  = new ArrayBuffer[R]() ++ getTerminals(t)
    cNonTerminals(l) = for (g <- getNonTerminals(t)) yield {
      new Generator(g.subTrees.map(labelId), g.builder)
    }
  }

  def getCTerminals(l: Int): ArrayBuffer[R] = {
    if (!isGenInit(l)) {
      fetchAndCompileGenerators(l)
    }
    cTerminals(l)
  }

  def getCNonTerminals(l: Int): Seq[CGen] = {
    if (!isGenInit(l)) {
      fetchAndCompileGenerators(l)
    }
    cNonTerminals(l)
  }

  def depthLabel(l: Int, depth: Int): Int = {
    depth*MAX_LABELS + l
  }

  val expectedSizes = new MutableHashMap[DepthLabel, Int]()

  val treesSizes    = new MutableHashMap[DepthLabel, Int]().withDefaultValue(0)
  val trees         = new MutableHashMap[DepthLabel, ArrayBuffer[R]]()
  val depthsGens    = new MutableHashMap[DepthLabel, (Array[DepthGen], Array[(Int, Int)])]()

  def nTreesOf(dl: DepthLabel): Int = {
    expectedSizes.get(dl) match {
      case Some(es) => es
      case None =>
        val depth = dl.depth
        val l = dl.label

        val res = if (depth == 0) {
          getCTerminals(l).size
        } else {
          val sdepth = depth-1

          var dgens = new ArrayBuffer[DepthGen]()

          var overallSum = 0;

          getCNonTerminals(l).foreach { g =>
            sumTo(sdepth, g.arity).foreach { sizes =>
              val subs = (g.subTrees zip sizes).toArray.map(dl => DepthLabel(dl._1, dl._2))
              val subSizes = subs.map { nTreesOf }
              val genSize = subSizes.product

              dgens += DepthGen(g, subs, subSizes, genSize)
              overallSum += genSize
            }
          }

          val orderedDgens = dgens.toArray.sortBy(-_.size)

          // Modulos computation:
          val modulos = new ArrayBuffer[(Int, Int)]()
          var min = 0
          var mid = -1
          var mod = dgens.size

          for (s <- dgens.toArray.map(_.size).sorted) {
            val threshold = (s-min)*mod;
            if (threshold == 0 && mid >= 0) {
              modulos(mid) = (modulos(mid)._1, mod-1)
              mod -= 1
            } else {
              if (mod != 1) {
                modulos += ((threshold, mod-1))
                mid += 1
              }
              mod -= 1
              min = s
            }
          }

          depthsGens += dl -> ((orderedDgens, modulos.toArray))


          overallSum
        }

        expectedSizes += dl -> res
        res
    }
  }
  def nTreesOf(l: T, depth: Int): Int = {
    nTreesOf(DepthLabel(labelId(l), depth))
  }

  case class DepthLabel(label: Int, depth: Int) {
    override def hashCode: Int = {
      depth*MAX_LABELS + label
    }
  }

  /**
   * gen: The compiled generator used by this non-terminal
   *
   * sub: an array of subtree depth-labels
   *
   * ds: the size of each depth-label
   *
   * size: the size of this generator
   */

  case class DepthGen(gen: CGen, sub: Array[DepthLabel], ds: Array[Int], size: Int) {
    override def toString = {
      "["+gen.subTrees.map(idsLabels).mkString(", ")+", "+sub.mkString(".")+"]"
    }
  }

  def depthGenSelect(dl: DepthLabel, seed: Int): (DepthGen, Int) = {
    /**
     * We know dgs is ordered DESC in size of generators,
     * Plan is to, from the seed, pick DG by a series of / and %
     */
     val (dgs, modulos) = depthsGens(dl)

     var m = dgs.size
     var s = seed
     var bs = 0
     var mi = 0
     var continue = modulos.nonEmpty;

     while(continue && mi < modulos.size) {
       val (limit, newmod) = modulos(mi)
       if (s >= limit) {
        bs  += limit/m
        m    = newmod
        s   -= limit
        mi  += 1
       } else {
        continue = false;
       }
     }

     val dg = dgs(s % m)
     val sseed = s/m+bs
     (dg, sseed)
  }

  def genExpr(gen: CGen, sub: Array[DepthLabel], ds: Array[Int], seed: Int): R = {
    var res = new ArrayBuffer[Int]()
    var s = seed
    for (d <- ds) {
      res += s % d
      s = s / d
    }

    val sexprs = (sub zip res).map { case (dl, s) => getTree(dl, s) }
    gen.builder(sexprs)
  }

  def getTree(dl: DepthLabel, seed: Int): R = {

    val available = treesSizes(dl)
    if (available > seed) {
      //println("Fetching tree #"+seed+"("+available+") @"+depth+"("+dl+") for "+idsLabels(l))
      trees(dl)(seed)
    } else {
      if (available == 0) {
        //println("Initializing at @"+depth+" for "+idsLabels(l))
        trees(dl) = new ArrayBuffer[R]();
      }

      if (seed > 0 && available < seed) {
        //println("Backtracking ?!?")
        for (i <- available until seed) {
          getTree(dl, i)
        }
      }

      if (dl.depth == 0) {
        val gs = getCTerminals(dl.label)
        trees += dl -> gs
        treesSizes(dl) = gs.size
        if (seed < gs.size) {
          gs(seed)
        } else {
          throw new BonsaiException("Can't produce tree #"+seed+" @"+dl.depth+" for "+idsLabels(dl.label))
        }
      } else {
        //println("Producing tree #"+seed+" @"+dl.depth+" for "+idsLabels(dl.label))
        val (DepthGen(gen, sub, ds, _), sseed) = depthGenSelect(dl, seed)

        val res = genExpr(gen, sub, ds, sseed)
        //println("=> "+res)
        treesSizes(dl) += 1
        trees(dl) += res
        //assert(trees(dl).size == treesSizes(dl))

        res
      }

    }
  }

  def getTree(l: Int, absSeed: Int): R = {
    var d = 0
    var seed = absSeed

    var continue = true
    do {
      var n = nTreesOf(DepthLabel(l, d))
      if (seed >= n) {
        seed -= n
        d += 1
      } else {
        continue = false
      }
    } while(continue);

    getTree(DepthLabel(l, d), seed)
  }

  def getTree(l: T, absSeed: Int): R = {
    getTree(labelId(l), absSeed)
  }

  def iterator(t: T): Iterator[R] = {
    val lab = labelId(t)

    new Iterator[R] {
      var depth = 0
      var absSeed = 0
      var seed = 0
      var dl = DepthLabel(lab, depth)
      var nTrees = nTreesOf(dl)

      def hasNext = {
        var expanded = 0
        while (seed == nTrees && expanded < MAX_EXPANDS) {
          seed = 0
          depth += 1
          dl = DepthLabel(lab, depth)
          nTrees = nTreesOf(dl)
          expanded += 1
        }
        seed != nTrees
      }
      def next = {
        val r = getTree(dl, seed)
        seed += 1
        absSeed += 1
        r
      }
    }
  }

  val sumTos: MutableMap[Int, Seq[Seq[Int]]] = new MutableHashMap()

  def sumTo(sum: Int, arity: Int): Seq[Seq[Int]] = sumTos.getOrElse(sum*100+arity, {
    val res = if (arity == 1) {
      Seq(Seq(sum))
    } else {
      (0 to sum).flatMap{n => sumTo(sum-n, arity-1).map( r => n +: r) }
    }
    sumTos += (sum*100+arity -> res)
    res
  })
}
