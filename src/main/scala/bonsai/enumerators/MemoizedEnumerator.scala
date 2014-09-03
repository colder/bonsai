package bonsai
package enumerators

import scala.collection.mutable.{ArrayBuffer, BitSet, Map => MutableMap, HashMap => MutableHashMap}

class MemoizedEnumerator[T, R](grammar: T => Seq[Generator[T, R]]) {
  type Gen  = Generator[T, R]
  type CGen = CompiledGenerator[R]

  //var tsNTrees   = ArrayBuffer[Long]()
  //var tsGetTrees = ArrayBuffer[Long]()
  //var tsProbe    = ArrayBuffer[Long]()

  // Maximum distinct labels
  val MAX_LABELS = 1024;

  // Maximum consecutive expands without finding new trees
  val MAX_EXPANDS = 5;

  // Compile labels for more efficient processing
  private[this] var nextLabelId = -1;
  private[this] var labelsIds = Map[T, Int]()
  private[this] var idsLabels = Map[Int, T]()
  val grounds:  ArrayBuffer[ArrayBuffer[R]] = new ArrayBuffer()
  val builders: ArrayBuffer[Seq[CGen]]     = new ArrayBuffer()

  def labelId(l: T): Int = {
    labelsIds.getOrElse(l, {
      nextLabelId += 1
      if (nextLabelId >= MAX_LABELS) {
        throw new BonsaiException("Exceeded the number of labels available")
      }

      grounds += new ArrayBuffer[R]()
      builders += Nil

      labelsIds  += l -> nextLabelId
      idsLabels += nextLabelId -> l

      nextLabelId
    })
  }

  // Compiled generators (grounds + builders) for each compiled label
  val isGenInit: BitSet = new BitSet()

  def fetchAndCompileGenerators(l: Int): Unit = {
    val t = idsLabels(l)
    val (gs, bs) = grammar(t).partition(_.arity == 0)

    //println("Fetching grammar for "+t)

    isGenInit += l
    grounds(l)  = new ArrayBuffer[R]() ++ gs.map(_.builder(Seq()))
    builders(l) = bs.map(g => CompiledGenerator(g.subTrees.map(labelId), g.builder))
  }

  def getGrounds(l: Int): ArrayBuffer[R] = {
    if (!isGenInit(l)) {
      fetchAndCompileGenerators(l)
    }
    grounds(l)
  }

  def getBuilders(l: Int): Seq[CGen] = {
    if (!isGenInit(l)) {
      fetchAndCompileGenerators(l)
    }
    builders(l)
  }

  def depthLabel(l: Int, depth: Int): Int = {
    depth*MAX_LABELS + l
  }

  val expectedSizes: MutableMap[Int, Int] = new MutableHashMap[Int, Int]()
  //val genSizesCache: MutableMap[(BitSet, Int), Int] = new MutableHashMap[Int, Int]()

  def nTreesOf(l: Int, depth: Int): Int = {
    val dl = depthLabel(l, depth)
    expectedSizes.get(dl) match {
      case Some(es) => es
      case None =>
        val res = if (depth == 0) {
          getGrounds(l).size
        } else {
          //val ts = System.currentTimeMillis
          val sdepth = depth-1

          var dgens = new ArrayBuffer[DepthGen]()

          var overallSum = 0;

          //val ts3 = System.currentTimeMillis
          getBuilders(l).foreach { g =>
            sumTo(sdepth, g.arity).foreach { sizes =>
              val subs = (g.subTrees zip sizes).toArray
              val subSizes = subs.map { case (t, d) => nTreesOf(t, d) }
              val genSize = subSizes.product

              dgens += DepthGen(g, subs, subSizes, genSize)
              overallSum += genSize
            }
          }
          //tsProbe += System.currentTimeMillis-ts3

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

          //tsNTrees += System.currentTimeMillis-ts

          overallSum
        }

        expectedSizes += dl -> res
        res
    }
  }
  def nTreesOf(l: T, depth: Int): Int = {
    nTreesOf(labelId(l), depth)
  }

  case class DepthGen(gen: CGen, sub: Array[(Int, Int)], ds: Array[Int], size: Int)

  val treesSizes: MutableMap[Int, Int]               = new MutableHashMap[Int, Int]().withDefaultValue(0)
  val trees: MutableMap[Int, ArrayBuffer[R]]         = new MutableHashMap[Int, ArrayBuffer[R]]()
  val depthsGens: MutableMap[Int, (Array[DepthGen], Array[(Int, Int)])] = new MutableHashMap[Int, (Array[DepthGen], Array[(Int, Int)])]()

  def depthGenSelect(dl: Int, seed: Int): (DepthGen, Int) = {
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

  def genExpr(gen: CGen, sub: Array[(Int, Int)], ds: Array[Int], seed: Int): R = {
    var res = new ArrayBuffer[Int]()
    var s = seed
    for (d <- ds) {
      res += s % d
      s = s / d
    }

    val sexprs = (sub zip res).map { case ((l, d), s) => getTree(l, d, s) }
    gen.builder(sexprs)
  }

  def getTree(l: Int, depth: Int, seed: Int): R = {
    val dl = depthLabel(l, depth)
    val available = treesSizes(dl)
    if (available > seed) {
      //println("Fetching tree #"+seed+"("+available+") @"+depth+"("+dl+") for "+idsLabels(l))
      trees(dl)(seed)
    } else {
      //val ts = System.currentTimeMillis
      if (available == 0) {
        //println("Initializing at @"+depth+" for "+idsLabels(l))
        trees(dl) = new ArrayBuffer[R]();
      }

      if (seed > 0 && available < seed) {
        //println("Backtracking ?!?")
        for (i <- available until seed) {
          getTree(l, depth, i)
        }
      }

      if (depth == 0) {
        val gs = getGrounds(l)
        trees += dl -> gs
        treesSizes(dl) = gs.size
        if (seed < gs.size) {
          //tsGetTrees += System.currentTimeMillis-ts
          gs(seed)
        } else {
          throw new BonsaiException("Can't produce tree #"+seed+" @"+depth+" for "+idsLabels(l))
        }
      } else {
        //println("Producing tree #"+seed+" @"+depth+" for "+idsLabels(l))
        val (dg, sseed) = depthGenSelect(dl, seed)
        val DepthGen(gen, sub, ds, _) = dg

        val res = genExpr(gen, sub, ds, sseed)
        treesSizes(dl) += 1
        trees(dl) += res
        //assert(trees(dl).size == treesSizes(dl))

        //tsGetTrees += System.currentTimeMillis-ts
        res
      }

    }
  }

  def getTree(l: Int, absSeed: Int): R = {
    var d = 0
    var seed = absSeed

    var continue = true
    do {
      var n = nTreesOf(l, d)
      if (seed >= n) {
        seed -= n
        d += 1
      } else {
        continue = false
      }
    } while(continue);

    getTree(l, d, seed)
  }

  def getTree(l: T, absSeed: Int): R = {
    getTree(labelId(l), absSeed)
  }

  //def printStats = {
  //  def printStat(name: String, tss: ArrayBuffer[Long]) = {
  //    val n = tss.size
  //    val (sum, avg, min, max) = if (n == 0) {
  //      (0l, 0d,0l,0l)
  //    } else {
  //      (tss.sum, tss.sum*1d/n, tss.min, tss.max)
  //    }

  //    println(f"$name%-20s $sum%-4d (n: $n%-3d, min: $min%4d, avg: $avg%,2f, max: $max%4d)")

  //  }

  //  printStat("getTree",  tsGetTrees)
  //  printStat("nTreesOf", tsNTrees)
  //  printStat("probe",    tsProbe)
  //}

  def iterator(t: T): Iterator[R] = {
    val lab = labelId(t)

    new Iterator[R] {
      var depth = 0
      var absSeed = 0
      var seed = 0
      var nTrees = nTreesOf(lab, depth)

      def hasNext = {
        var expanded = 0
        while (seed == nTrees && expanded < MAX_EXPANDS) {
          seed = 0
          depth += 1
          nTrees = nTreesOf(lab, depth)
          //println("Expanding to depth "+depth+": "+nTrees+" trees");
          expanded += 1
        }
        seed != nTrees
      }
      def next = {
        val r = getTree(lab, depth, seed)
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
