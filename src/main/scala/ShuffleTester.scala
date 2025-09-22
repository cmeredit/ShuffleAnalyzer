import java.io.{BufferedWriter, File, FileWriter}

object ShuffleTester extends App {





  sealed trait DistOption
  case object Binomial extends DistOption
  case object Uniform extends DistOption






//  val numShuffles: Int = 4
//  val numShuffles: Int = 7
  val numShuffles: Int = 15
//  val deckSize = 177
  val deckSize = 100
//  val numTrials = 1000
  val numTrials = 30_000
//  val numTrials = 1
  type Deck = Vector[Int]

  // See chapter 8.3 of "Markov Chains and Mixing Times", Second Edition by Levin & Peres (https://pages.uoregon.edu/dlevin/MARKOV/)
  // Or see Lemma 3 of Bayer & Diaconis 1992
  //
  // Reverse riffle: Assign each card a 0 or 1 with equal probability, then sort the deck by the assigned value (preserving all other order relationships)
  // Riffle: Invert a reverse-riffle permutation.
  def reverseBinomialRiffleShuffle(deck: Deck): Deck = deck.map(n => (n, scala.util.Random.nextInt(2))).sortBy(_._2).map(_._1)
  def binomialRiffleShuffle(deck: Deck): Deck = deck.zip(reverseBinomialRiffleShuffle(deck)).sortBy(_._2).map(_._1)


  // Place card 1 in pile 1, card 2 in pile 2, ..., card n in pile n, card n+1 in pile 1, etc., until running out of cards.
  // Then stack the piles on each other in order.
  def pileShuffle(deck: Deck, numPiles: Int): Deck = deck.zipWithIndex.map({case (card, index) => (card, index % numPiles)}).sortBy(_._2).map(_._1)

  // A custom shuffling method a friend wanted analyzed for large decks.
  def travisShuffle(deck: Deck): Deck = {

    def partitionDeck(d: Deck, numGroups: Int): Map[Int, Deck] = {
      d.zipWithIndex
        // Associate each card with its group index. Vector[(Card, group index 0 ... numGroups - 1)]
        .map({case (card, index) => (card, index * numGroups / deck.length)})
        // Group the cards by their group index and strip off extra index instances. Map[Index, Group with that index]
        .groupBy(_._2).map({case (groupIndex, cardsInGroupWithIndices) => (groupIndex, cardsInGroupWithIndices.map(_._1))})
    }

    // split deck into 3 groups of appx equal size (equal in the case of deck.length divisible by 3)
    val (a1, a2, a3) = {
      val initialGroups = partitionDeck(deck, 3)

      (initialGroups(0), initialGroups(1), initialGroups(2))
    }

    val b = binomialRiffleShuffle(a1 ++ a2)
    val (b1, b2) = {
      val bGroups = partitionDeck(b, 2)

      (bGroups(0), bGroups(1))
    }

    val c = binomialRiffleShuffle(b1 ++ a3)
    val (c1, c2) = {
      val cGroups = partitionDeck(c, 2)

      (cGroups(0), cGroups(1))
    }

    // shuffle c1 and b2 together, then stick c2 on top
    binomialRiffleShuffle(c1 ++ b2) ++ c2

  }
//  saveLandscape(getShuffleLandscape(travisShuffle), f"Data/Obj/Sub1/travis${numTrials}Trials${numShuffles}Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  saveLandscape(getShuffleLandscape(x => scala.util.Random.shuffle(x)), f"Data/Obj/Sub1/perfect shuffle.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)



  def splitMashShuffle(deck: Deck, splitType: DistOption = Binomial, mashType: DistOption = Binomial, interleaveRetentionProbability: Double = 0.0): Deck = {

    val splitLocation: Int = splitType match {
      case Uniform => scala.util.Random.nextInt(deck.length)
      case Binomial => (1 until deck.length).map(_ => scala.util.Random.nextInt(2)).sum
    }

    val topShiftAmount: Int = mashType match {
      case Uniform => scala.util.Random.nextInt(deck.length)
      case Binomial => (1 until deck.length).map(_ => scala.util.Random.nextInt(2)).sum
    }



    val bottom: Deck = deck.drop(splitLocation)
    val top: Deck = deck.take(splitLocation)


    def interleave(firstDeck: Deck, secondDeck: Deck, retentionProbability: Double): Deck = {
      if (retentionProbability == 0.0) {
        firstDeck.zip(secondDeck).flatMap({case (l, r) => Vector(l, r)})
      } else if (retentionProbability == 1.0) {
        firstDeck ++ secondDeck
      } else {

        var firstIndex: Int = 0
        var secondIndex: Int = 0
        var takingFromFirstDeck: Boolean = true
        var numToTake: Int = 0
        var currentDeck: Deck = Vector()

        while (firstIndex < firstDeck.length && secondIndex < secondDeck.length) {
          numToTake = 1
          val relevantIndex = if (takingFromFirstDeck) firstIndex else secondIndex
          val relevantMax = if (takingFromFirstDeck) firstDeck.length else secondDeck.length
          while (scala.util.Random.nextDouble() < retentionProbability && relevantIndex + numToTake < relevantMax) {
            numToTake = numToTake + 1
          }
          if (takingFromFirstDeck) {
            currentDeck = currentDeck ++ firstDeck.slice(firstIndex, firstIndex + numToTake)
            firstIndex = firstIndex + numToTake
          } else {
            currentDeck = currentDeck ++ secondDeck.slice(secondIndex, secondIndex + numToTake)
            secondIndex = secondIndex + numToTake
          }
          takingFromFirstDeck = !takingFromFirstDeck
        }

        if (firstIndex < firstDeck.length) {
          currentDeck = currentDeck ++ firstDeck.slice(firstIndex, firstDeck.length)
        }
        if (secondIndex < secondDeck.length) {
          currentDeck = currentDeck ++ secondDeck.slice(secondIndex, secondDeck.length)
        }

//        println(firstDeck)
//        println(secondDeck)
//        println(currentDeck)
//        println(f"Indices: $firstIndex, $secondIndex")
//        println(f"${firstDeck.length}, ${secondDeck.length}, ${currentDeck.length}")
//        assert(false)

        assert(firstDeck.length + secondDeck.length == currentDeck.length)
        assert((firstDeck ++ secondDeck).toSet == currentDeck.toSet)
        currentDeck
      }
    }

    if (top.length < bottom.length) {
      if (topShiftAmount < top.length) { // top portion is partially interleaved with top of the bottom portion
        val (unaffectedTopPortion, affectedTopPortion) = top.splitAt(top.length - topShiftAmount)
//        val affectedTopPortion: Deck = top.drop(top.length - topShiftAmount)
        val (affectedBottomPortion, unaffectedBottomPortion) = bottom.splitAt(topShiftAmount)
//        val unaffectedBottomPortion: Deck = bottom.drop(topShiftAmount)

//        println("A")
//        unaffectedTopPortion ++ affectedBottomPortion.zip(affectedTopPortion).flatMap({case (l, r) => Vector(l, r)}) ++ unaffectedBottomPortion
        unaffectedTopPortion ++ interleave(affectedBottomPortion, affectedTopPortion, interleaveRetentionProbability) ++ unaffectedBottomPortion
      }
      else if (topShiftAmount < bottom.length) { // top portion is entirely contained within the bottom
        val bottomUnaffectedTop: Deck = bottom.take(topShiftAmount - top.length)
        val bottomAffected: Deck = bottom.slice(topShiftAmount - top.length, topShiftAmount)
        val bottomUnaffectedBottom: Deck = bottom.drop(topShiftAmount)

//        println("B")
//        bottomUnaffectedTop ++ bottomAffected.zip(top).flatMap({case (l, r) => Vector(l, r)}) ++ bottomUnaffectedBottom
        bottomUnaffectedTop ++ interleave(bottomAffected, top, interleaveRetentionProbability) ++ bottomUnaffectedBottom
      } else { // top portion is partially interleaved with the bottom of the bottom portion
        val (unaffectedBottomPortion, affectedBottomPortion) = bottom.splitAt(topShiftAmount-top.length)
//        val affectedBottomPortion: Deck = bottom.drop(topShiftAmount-top.length)3
        val (affectedTopPortion, unaffectedTopPortion) = top.splitAt(top.length - (topShiftAmount - bottom.length))

//        println("C")
//        unaffectedBottomPortion ++ affectedBottomPortion.zip(affectedTopPortion).flatMap({case (l, r) => Vector(l, r)}) ++ unaffectedTopPortion
        unaffectedBottomPortion ++ interleave(affectedBottomPortion, affectedTopPortion, interleaveRetentionProbability) ++ unaffectedTopPortion
      }
    } else {
      if (topShiftAmount < bottom.length) { // bottom partially interleaves with top
        val (unaffectedTopPortion, affectedTopPortion) = top.splitAt(top.length - topShiftAmount)
        val (affectedBottomPortion, unaffectedBottomPortion) = bottom.splitAt(topShiftAmount)

//        println("D")
//        unaffectedTopPortion ++ affectedBottomPortion.zip(affectedTopPortion).flatMap({case (l, r) => Vector(l, r)}) ++ unaffectedBottomPortion
        unaffectedTopPortion ++ interleave(affectedBottomPortion, affectedTopPortion, interleaveRetentionProbability) ++ unaffectedBottomPortion

      } else if (topShiftAmount < top.length) { // bottom fully inserted within top

        val firstUnaffectedTopPortion = top.take(top.length - topShiftAmount)
        val affectedTopPortion = top.slice(top.length - topShiftAmount, top.length - topShiftAmount + bottom.length)
        val secondUnaffectedTopPortion = top.takeRight(topShiftAmount - bottom.length)


//        println("E")
//        firstUnaffectedTopPortion ++ affectedTopPortion.zip(bottom).flatMap({case (l, r) => Vector(l, r)}) ++ secondUnaffectedTopPortion
        firstUnaffectedTopPortion ++ interleave(affectedTopPortion, bottom, interleaveRetentionProbability) ++ secondUnaffectedTopPortion

      } else { // bottom partially interleaves with top of top
        val (unaffectedBottomPortion, affectedBottomPortion) = bottom.splitAt(topShiftAmount-top.length)
        val (affectedTopPortion, unaffectedTopPortion) = top.splitAt(bottom.length - (topShiftAmount-top.length))

//        println("F")
//        unaffectedBottomPortion ++ affectedTopPortion.zip(affectedBottomPortion).flatMap({case (l, r) => Vector(l, r)}) ++ unaffectedTopPortion
        unaffectedBottomPortion ++ interleave(affectedTopPortion, affectedBottomPortion, interleaveRetentionProbability) ++ unaffectedTopPortion
      }
    }

  }






  def averageNeighborDistance(deck: Deck): Double = {

    (0 until deck.length-1).map(n => {

      val nLoc = deck.indexOf(n)
      val np1Loc = deck.indexOf(n+1)

      (scala.math.abs(nLoc - np1Loc)-1).toDouble

    }).sum / deck.length.toDouble

  }


  def histogramNeighborDistance(deck: Deck): Vector[(Int, Int)] = {
    val distances: IndexedSeq[Int] = (0 until deck.length-1).map(n => {

      val nLoc = deck.indexOf(n)
      val np1Loc = deck.indexOf(n+1)

      scala.math.abs(nLoc - np1Loc)-1

    })

    distances.groupBy(x => x).toVector.map({case (l, r) => (l, r.length)})
  }




//  (0 until 100).foreach(_ => {
//    val noob = noobShuffle((0 until 10).toVector)
//    println(noob)
//    println(averageNeighborDistance(noob))
//    val uniform = scala.util.Random.shuffle((0 until 10).toVector)
//    println(f"Uniform shuffle average neighbor distance: ${averageNeighborDistance(uniform)}")
//    assert(noob.toSet == (0 until 10).toSet)
//  })






  def getAverageNeighborDistance(shuffleMethod: Deck => Deck): Double = (0 until numTrials).map(_ => averageNeighborDistance(shuffleMethod((0 until deckSize).toVector))).sum / numTrials.toDouble

  def getHistogramNeighborDistance(shuffleMethod: Deck => Deck): Vector[(Int, Double)] = {
    val histograms: IndexedSeq[Vector[(Int, Int)]] = (0 until numTrials).map(trialNum => {

      if (trialNum % 3_000 == 0) {
        println(f"Working on trial ${trialNum+1}/$numTrials. ${trialNum.toDouble / numTrials.toDouble * 100.0}%2.2f%% complete.")
      }

      histogramNeighborDistance(shuffleMethod((0 until deckSize).toVector))
    })

    def mergeHistograms(hist1: Vector[(Int, Int)], hist2: Vector[(Int, Int)]): Vector[(Int, Int)] = (hist1 ++ hist2).groupBy(_._1).toVector.map({case (distNum, instances) => (distNum, instances.map(_._2).sum)})
    def cleanHistogram(hist: Vector[(Int, Int)]): Vector[(Int, Double)] = {

      // Make sure no distances are missing!
      val histWithAllDistancesRepresented: Vector[(Int, Int)] = mergeHistograms(hist, (0 until deckSize).toVector.map(x => (x, 0)))

      // Normalize the freq values to (0, 1)
//      val highestFreq: Int = histWithAllDistancesRepresented.map(_._2).max

      histWithAllDistancesRepresented.map({case (x, freq) => (x, freq.toDouble / (deckSize * numTrials).toDouble)}).sortBy(_._1)

    }

    val result = cleanHistogram(histograms.reduce(mergeHistograms))

    println(result)
    println(result.map({case (dist, prop) => dist.toDouble * prop}).sum)
    println(result.map(_._2).sum)


    result

  }

//  val averageUniformNeighborDistance = getAverageNeighborDistance(scala.util.Random.shuffle(_))
//  val averageNoobNeighborDistance = getAverageNeighborDistance(noobShuffle)
//  val averageRiffleShuffleDistance = getAverageNeighborDistance(binomialRiffleShuffle)

//  println(f"Number of trials: $numTrials")
//  println(f"Deck size: $deckSize")
//  println(f"Average neighbor distance with a uniform shuffle: $averageUniformNeighborDistance")
//  println(f"Average neighbor distance with a split-mash shuffle: $averageNoobNeighborDistance")
//  println(f"Average neighbor distance with a binomial riffle shuffle: $averageRiffleShuffleDistance")

//  def repeatedly(shuffleMethod: Deck => Deck, numTimes: Int): Deck => Deck = v => {if (numTimes > 1) shuffleMethod(repeatedly(shuffleMethod, numTimes-1)(v)) else shuffleMethod(v)}
  def repeatedly(shuffleMethod: Deck => Deck, numTimes: Int): Deck => Deck = v => {if (numTimes > 0) shuffleMethod(repeatedly(shuffleMethod, numTimes-1)(v)) else v}

  def repeatedlyChained(firstMethod: Deck => Deck, secondMethod: Deck => Deck, maxTimesForFirstMethod: Int, numTimes: Int): Deck => Deck = {
    if (numTimes <= maxTimesForFirstMethod) {
      repeatedly(firstMethod, numTimes)
    } else {
      v => repeatedly(secondMethod, numTimes - maxTimesForFirstMethod)(repeatedly(firstMethod, maxTimesForFirstMethod)(v))
    }
  }

//  println("Repeated riffle shuffles")
////  (1 to 40).foreach(n => println(f"Riffle shuffle dist after $n shuffles: ${getAverageNeighborDistance(repeatedly(binomialRiffleShuffle, n))}"))
//  println("0, 0")
//  (1 to 40).foreach(n => println(f"$n, ${getAverageNeighborDistance(repeatedly(binomialRiffleShuffle, n))}"))
//  println("Repeated split-mash shuffles")
//  println("0, 0")
//  (1 to 40).foreach(n => {
//    if (n > 10) numTrials = 10_000
//    println(f"$n, ${getAverageNeighborDistance(repeatedly(noobShuffle, n))}")
//  })



  def saveCircle(numPoints: Int, filename: String): Unit = {

    val edgePoints: Vector[(Double, Double, Double)] = (0 until numPoints).toVector.map(n => {
      (scala.math.cos(2.0 * scala.math.Pi * n.toDouble / numPoints.toDouble), 0.0, scala.math.sin(2.0 * scala.math.Pi * n.toDouble / numPoints.toDouble))
    })
    val center: (Double, Double, Double) = (0.0, 0.0, 0.0)
    val centerIndex: Int = edgePoints.length
    val allPoints: Vector[(Double, Double, Double)] = edgePoints.appended(center)

    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))

    bw.write(f"# Circle with $numPoints points!\n")

    // Establish the vertices of the circle
    allPoints.zipWithIndex.foreach({ case (point, index) =>
      bw.write(f"# V${index+1}\n")
      bw.write(f"v ${point._1}%.8f ${point._2}%.8f ${point._3}%.8f\n")
    })

    // Manually go around the circle, stitching two adjacent edge vertices and the center together into a triangle
    edgePoints.indices.foreach(index => {
      val upperIndex = (index + 1) % edgePoints.length
      bw.write(f"f ${index+1} ${upperIndex+1} ${centerIndex+1}\n")
    })


    bw.close()

  }




  def saveLandscape(rows: Vector[Vector[Double]], filename: String, xSpacing: Double = 1.0, zSpacing: Double = 1.0, yScale: Double = 1.0): Unit = {

    // Give us SOMETHING!
    assert(rows.nonEmpty)
    assert(rows.head.nonEmpty)
    // All rows must have the same length
//    rows foreach println
    rows.map(_.length) foreach println
    assert(rows.map(_.length).distinct.length == 1)

//    val numRows: Int = rows.length
    val numColumns: Int = rows.head.length

    def getPointObjIndex(rowIndex: Int, colIndex: Int): Int = rowIndex * numColumns + colIndex + 1

    val indexedPointHeights: Vector[(Vector[(Double, Int)], Int)] = rows.map(_.zipWithIndex).zipWithIndex

    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))

    // Write header
    bw.write(f"# Generated by saveLandscape\n")

    // Write vertices
    indexedPointHeights.foreach({case (row, rowIndex) => row.map({case (pointHeight, colIndex) =>
      bw.write(f"# V${getPointObjIndex(rowIndex, colIndex)}\n")
      bw.write(f"v ${rowIndex.toDouble * xSpacing}%.8f ${pointHeight * yScale}%.8f ${colIndex.toDouble * zSpacing}%.8f\n")
    })})

    // Form faces!!!
    indexedPointHeights.sliding(2).foreach(v => {

      val (curRow, curRowIndex) = v(0)
      val (nextRow, nextRowIndex) = v(1)

      curRow.zip(nextRow).sliding(2).foreach(w => {
        val ((_, aColIndex),(_, dColIndex)) = w(0)
        val ((_, bColIndex), (_, cColIndex)) = w(1)
        val aObjIndex: Int = getPointObjIndex(curRowIndex, aColIndex)
        val bObjIndex: Int = getPointObjIndex(curRowIndex, bColIndex)
        val cObjIndex: Int = getPointObjIndex(nextRowIndex, cColIndex)
        val dObjIndex: Int = getPointObjIndex(nextRowIndex, dColIndex)

        bw.write(f"f $aObjIndex $bObjIndex $cObjIndex $dObjIndex\n")
      })

    })

    bw.close()

  }







//  getHistogramNeighborDistance(repeatedly(binomialRiffleShuffle, 1)) foreach println
//
//
//
//  val riffleLandscape: Vector[Vector[Double]] = (0 until 15).toVector.map(n => getHistogramNeighborDistance(repeatedly(binomialRiffleShuffle, n)).map(p => scala.math.log(1.0 + p._2)))
//
//  saveLandscape(riffleLandscape, "Data/Obj/RiffleHist15TrialsTest2.obj", xSpacing = 1.0 / 15.0, zSpacing = 1.0 / 100.0)
//
//
//  val noobLandscape: Vector[Vector[Double]] = (0 until 15).toVector.map(n => getHistogramNeighborDistance(repeatedly(splitMashShuffle(_), n)).map(p => p._2))
//
//  saveLandscape(noobLandscape, "Data/Obj/BinomsplitUnimashHistogramReg.obj", xSpacing = 1.0 / 15.0, zSpacing = 1.0 / 100.0, yScale = 10.0)
//
//
//  def getShuffleLandscape(method: Deck => Deck): Vector[Vector[Double]] = (0 until numShuffles).toVector.map(n => getHistogramNeighborDistance(repeatedly(method, n)).map(p => p._2))
//  def getShuffleLandscapeChained(firstMethod: Deck => Deck, secondMethod: Deck => Deck, maxTimesForFirstMethod: Int): Vector[Vector[Double]] = (0 until numShuffles).toVector.map(n => getHistogramNeighborDistance(repeatedlyChained(firstMethod, secondMethod, maxTimesForFirstMethod, n)).map(p => p._2))

  def getShuffleLandscape(method: Deck => Deck): Vector[Vector[Double]] = (1 to numShuffles).toVector.map(n => getHistogramNeighborDistance(repeatedly(method, n)).map(p => p._2))
  def getShuffleLandscapeChained(firstMethod: Deck => Deck, secondMethod: Deck => Deck, maxTimesForFirstMethod: Int): Vector[Vector[Double]] = (1 to numShuffles).toVector.map(n => getHistogramNeighborDistance(repeatedlyChained(firstMethod, secondMethod, maxTimesForFirstMethod, n)).map(p => p._2))





  // Old main running code
//  saveLandscape(getShuffleLandscape(splitMashShuffle(_, Uniform, Uniform)), "Data/Obj/Sub1/UniformSplitUniformMash30000Trials15Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  saveLandscape(getShuffleLandscape(splitMashShuffle(_, Uniform, Binomial)), "Data/Obj/Sub1/UniformSplitBinomialMash30000Trials15Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  saveLandscape(getShuffleLandscape(splitMashShuffle(_, Binomial, Uniform)), "Data/Obj/Sub1/BinomialSplitUniformMash30000Trials15Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  saveLandscape(getShuffleLandscape(splitMashShuffle(_, Binomial, Binomial)), "Data/Obj/Sub1/BinomialSplitBinomialMash30000Trials15Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  saveLandscape(getShuffleLandscape(binomialRiffleShuffle), "Data/Obj/Sub1/BinomialRiffle30000Trials15Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)



  saveLandscape(getShuffleLandscape(binomialRiffleShuffle), "Data/Obj/Sub1/BinomialRiffle30000Trials15Shuffles Corrected.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)




  // 2025 post cherry springs
//  saveLandscape(getShuffleLandscape(pileShuffle(_, 5)), "Data/Obj/Sub1/5Pile1Trial100Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  val fivePile: Deck => Deck = pileShuffle(_, 5)
  val binomialSplitBinomialMash: Deck => Deck = splitMashShuffle(_)
//  saveLandscape(getShuffleLandscapeChained(fivePile, binomialSplitBinomialMash, 3), "Data/Obj/Sub1/30000 Trials 3 5-Piles 7 BinBin SplitMash.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  saveLandscape(getShuffleLandscapeChained(binomialSplitBinomialMash, fivePile, 7), "Data/Obj/Sub1/30000 Trials 7 BinBin SplitMash 3 5-Piles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)


  val sixPile: Deck => Deck = v => {
//    println("alsdkjfasdf")

    pileShuffle(v, 6)
  }
  val stickyBinomialSplitBinomialMash: Deck => Deck = splitMashShuffle(_, Binomial, Binomial, 0.5)
//  saveLandscape(getShuffleLandscapeChained(sixPile, stickyBinomialSplitBinomialMash, 1), "Data/Obj/Sub1/Rob 6 pile and 3 binbin splitmash.obj", xSpacing = 1.0 / 6.0 /*Force same spacing as 7-shuffle next line*/, zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  saveLandscape(getShuffleLandscape(binomialSplitBinomialMash), "Data/Obj/Sub1/BinomialSplitBinomialMash30000Trials7Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)
//  saveLandscape(getShuffleLandscape(stickyBinomialSplitBinomialMash), "Data/Obj/Sub1/StickyBinomialSplitBinomialMash30000Trials7Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)

//  saveLandscape(getShuffleLandscape(binomialSplitBinomialMash), "Data/Obj/Sub1/BinomialSplitBinomialMash30000Trials10Shuffles.obj", xSpacing = 1.0 / (numShuffles.toDouble-1.0), zSpacing = 1.0 / (deckSize.toDouble-1.0), yScale = 10.0)














//  saveCircle(100, "Data/Obj/Circle.obj")

//  saveLandscape(Vector(Vector(0.0, 0.0), Vector(1.0, 1.0), Vector(0.0, 0.0)), "Data/Obj/RidgeLandscapeTest.obj")

//  val sineWave: Vector[Double] = (0 until 100).toVector.map(n => scala.math.sin(2.0 * scala.math.Pi * n.toDouble / 100.0))
//  val rapidSineWave: Vector[Double] = (0 until 100).toVector.map(n => scala.math.sin(4.0 * 2.0 * scala.math.Pi * n.toDouble / 100.0))
  //  saveLandscape(Vector(sineWave, rapidSineWave, sineWave, rapidSineWave), "Data/Obj/sines.obj", xSpacing = 1.0 / 3.0, zSpacing = 1.0 / 100.0, yScale = 0.1)

//  def getSines(numPeriods: Double, numPoints: Int): Vector[Double] = (0 until numPoints).toVector.map(n => scala.math.sin(numPeriods * 2.0 * scala.math.Pi * n.toDouble / numPoints.toDouble))

//  saveLandscape(Vector(getSines(0, 100), getSines(.5, 100), getSines(1, 100), getSines(1.5, 100), getSines(2, 100), getSines(2.5, 100), getSines(3, 100), getSines(3.5, 100), getSines(4, 100)),
//  "Data/Obj/increasingSineFrequencyMid.obj", xSpacing = 1.0 / 9.0, zSpacing = 1.0 / 100.0, yScale = 0.1)



//  val initPeriods: Int = 0
//  val finalPeriods: Int = 16
//  val innerSteps: Int = 10
//  val numSteps: Int = (finalPeriods - initPeriods) * innerSteps
//  val numPointsPerWave: Int = 400
//
//  val smoothSineRows: Vector[Vector[Double]] = (0 until numSteps).toVector.map(rowNum => {
//
//    val numPeriods: Double = initPeriods.toDouble + (rowNum.toDouble / numSteps.toDouble) * (finalPeriods.toDouble - initPeriods.toDouble)
//
//    getSines(numPeriods, numPointsPerWave)
//
//  })
//
//  saveLandscape(smoothSineRows, "Data/Obj/SuperSmoothSine2.obj", xSpacing = 1.0 / (numSteps.toDouble), zSpacing = 1.0 / numPointsPerWave.toDouble, yScale = 0.1)

}
