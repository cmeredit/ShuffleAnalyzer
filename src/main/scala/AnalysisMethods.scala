import ShuffleMethods.{Deck, repeatedly, repeatedlyChained}

object AnalysisMethods {


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

  def getAverageNeighborDistance(shuffleMethod: Deck => Deck, initialDeck: Deck, numTrials: Int): Double = (0 until numTrials).map(_ => averageNeighborDistance(shuffleMethod(initialDeck))).sum / numTrials.toDouble

  def getHistogramNeighborDistance(shuffleMethod: Deck => Deck, initialDeck: Deck, numTrials: Int): Vector[(Int, Double)] = {
    val histograms: IndexedSeq[Vector[(Int, Int)]] = (0 until numTrials).map(trialNum => {

      val updateCadence = numTrials / 5
      if (trialNum % updateCadence == 0) {
        println(f"Working on trial ${trialNum+1}/$numTrials. ${trialNum.toDouble / numTrials.toDouble * 100.0}%2.2f%% complete.")
      }

      histogramNeighborDistance(shuffleMethod(initialDeck))
    })

    def mergeHistograms(hist1: Vector[(Int, Int)], hist2: Vector[(Int, Int)]): Vector[(Int, Int)] = (hist1 ++ hist2).groupBy(_._1).toVector.map({case (distNum, instances) => (distNum, instances.map(_._2).sum)})
    def cleanHistogram(hist: Vector[(Int, Int)]): Vector[(Int, Double)] = {

      // Make sure no distances are missing!
      val histWithAllDistancesRepresented: Vector[(Int, Int)] = mergeHistograms(hist, initialDeck.map(x => (x, 0)))

      // Normalize the freq values to (0, 1)
//      val highestFreq: Int = histWithAllDistancesRepresented.map(_._2).max

      histWithAllDistancesRepresented.map({case (x, freq) => (x, freq.toDouble / (initialDeck.length * numTrials).toDouble)}).sortBy(_._1)

    }

    val result = cleanHistogram(histograms.reduce(mergeHistograms))

//    println(result)
//    println(result.map({case (dist, prop) => dist.toDouble * prop}).sum)
//    println(result.map(_._2).sum)


    result

  }

  def getNeighborDistanceHeightmap(method: Deck => Deck, numShuffles: Int, initialDeck: Deck, numTrials: Int): Vector[Vector[Double]] = (1 to numShuffles).toVector.map(n => getHistogramNeighborDistance(repeatedly(method, n), initialDeck, numTrials).map(p => p._2))

  def getShuffleLandscapeChained(firstMethod: Deck => Deck, secondMethod: Deck => Deck, maxTimesForFirstMethod: Int, numShuffles: Int, initialDeck: Deck, numTrials: Int): Vector[Vector[Double]] = (1 to numShuffles).toVector.map(n => getHistogramNeighborDistance(repeatedlyChained(firstMethod, secondMethod, maxTimesForFirstMethod, n), initialDeck, numTrials).map(p => p._2))

}
