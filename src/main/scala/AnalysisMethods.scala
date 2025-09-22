import ShuffleMethods.{Deck, repeatedly, repeatedlyChained}

object AnalysisMethods {


  private def averageNeighborDistance(deck: Deck): Double = {

    (0 until deck.length-1).map(n => {

      val nLoc = deck.indexOf(n)
      val np1Loc = deck.indexOf(n+1)

      (scala.math.abs(nLoc - np1Loc)-1).toDouble

    }).sum / deck.length.toDouble

  }

  def getAverageNeighborDistance(shuffleMethod: Deck => Deck, initialDeck: Deck, numTrials: Int): Double = (0 until numTrials).map(_ => averageNeighborDistance(shuffleMethod(initialDeck))).sum / numTrials.toDouble

  private def histogramNeighborDistance(deck: Deck): Vector[(Int, Int)] = {
    val distances: IndexedSeq[Int] = (0 until deck.length-1).map(n => {

      val nLoc = deck.indexOf(n)
      val np1Loc = deck.indexOf(n+1)

      scala.math.abs(nLoc - np1Loc)-1

    })

    distances.groupBy(x => x).toVector.map({case (l, r) => (l, r.length)})
  }

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

  def getNeighborDistanceHeightmap(firstMethod: Deck => Deck, secondMethod: Deck => Deck, maxTimesForFirstMethod: Int, numShuffles: Int, initialDeck: Deck, numTrials: Int): Vector[Vector[Double]] = (1 to numShuffles).toVector.map(n => getHistogramNeighborDistance(repeatedlyChained(firstMethod, secondMethod, maxTimesForFirstMethod, n), initialDeck, numTrials).map(p => p._2))

  // Might not use these. Keep them for now just in case
  //
  // Helper function for treating 1-D collections as 3-D collections.
  // Treat the collection as a list of xy-Layers (or z-slices, whatever).
  // Treat each xy-Layer as a list of x-Rows.
  // E.g., the indices of a 2x2x3 box will be:
  //
  // (0, 0, 0), (1, 0, 0), (0, 1, 0), (1, 1, 0),
  // (0, 0, 1), (1, 0, 1), (0, 1, 1), (1, 1, 1),
  // (0, 0, 2), (1, 0, 2), (0, 1, 2), (1, 1, 2)
  def get[A](collection: Vector[A], coordinates: (Int, Int, Int), collectionDims: (Int, Int, Int)): A = {
    val (i, j, k) = coordinates
    val (xDim, yDim, zDim) = collectionDims
    assert(collection.length == xDim * yDim * zDim)

    val xyLayerSize = xDim * yDim

    // k: Number of complete xy-Layers
    // j: Number of complete x-Layers (rows)
    // i: Number of additional elements
    val flatIndex = k * xyLayerSize + j * xDim + i
    assert(flatIndex < collection.length)

    collection(flatIndex)
  }
  def get[A](collection: Vector[A], coordinates: (Int, Int), collectionDims: (Int, Int)): A = get(collection, (coordinates._1, coordinates._2, 1), (collectionDims._1, collectionDims._2, 1))

  type TransitionMatrix = Map[(Int, Int), Double]
  def getTransitionMatrix(shuffleMethod: Deck => Deck, initialDeck: Deck, numShuffles: Int, numTrials: Int): TransitionMatrix = {

    // I'm lazy, so ensure that we only use decks of the form {0, 1, ..., deck size - 1}.
    assert(initialDeck.sorted == initialDeck.indices.toVector)

    // Keys: (source card, target card) pairs
    // Values: Number of times that transition was observed
    val transitionCounts: Map[(Int, Int), Int] = if (numShuffles <= 0) {
      // If we don't shuffle at all, then just return the identity matrix
      initialDeck.map(card => (card, card) -> numTrials).toMap.withDefaultValue(0)
    } else {

      def doTrial(): Vector[(Int, Int)] = {
        val shuffledDeck = repeatedly(shuffleMethod, numShuffles)(initialDeck)
        initialDeck.zip(shuffledDeck)
      }

      (0 until numTrials).flatMap(_ => doTrial()).groupBy(x => x).map({case (key, instances) => (key, instances.length)})

    }

    // In order to ensure that mesh creation goes smoothly later, we're also going to record all possible transitions that were not observed.
    // With a good shuffling method and a decent number of trials, this collection should be empty. However, sometimes it's nice to
    // visualize a crappy "shuffling" method like pile shuffling, or another deterministic, potentially intransitive, method.
    val unobservedTransitions: Vector[(Int, Int)] = for (
      source <- initialDeck;
      target <- initialDeck if !transitionCounts.contains((source, target))
    ) yield (source, target)
    val cleanedTransitionCounts: Map[(Int, Int), Int] = transitionCounts ++ unobservedTransitions.map(transition => (transition, 0)).toMap

    // Each card is observed in one transition per trial (every card goes to exactly one place).
    // Therefore, the number of transitions per source card is exactly numTrials.
    // For this reason, we can just scale down the transition counts by the number of trials.
    // E.g., if the (0, 1) transition was observed 5 times in 10 trials, there's a 50% chance that card 0 goes to position 1.
    cleanedTransitionCounts.map({case (key, transitionCount) => (key, transitionCount.toDouble / numTrials.toDouble)})

  }
  def getTransitionRows(transitionMatrix: TransitionMatrix): Vector[Vector[Double]] = {
    // keys: Source card
    // Values: Probability of transition from source to target
    val transitionsBySource: Map[Int, Vector[(Int, Double)]] = transitionMatrix.toVector.groupBy(_._1._1).map({case (source, instances) => (source, instances.map({case ((_, target), probability) => (target, probability)}).sortBy(_._1))})
    // Convert to vector, sort by source card, then extract just the transition probabilities.
    transitionsBySource.toVector.sortBy(_._1).map(_._2.map(_._2))
  }
}
