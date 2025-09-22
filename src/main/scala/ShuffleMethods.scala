object ShuffleMethods {

  // Distribution options. Relevant to split/mash shuffling below.
  sealed trait DistOption
  case object Binomial extends DistOption
  case object Uniform extends DistOption

  // Type alias for readability.
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


  // Split the deck into two blocks, then mash the two blocks together.
  // There are various mashing states handled through exhaustion below.
  //
  // Here's my artistic rendition of one possible split/mash:
  // Initial deck of 10 cards: CCCCCCCCCC
  // Split deck:               AAAAAAABBB
  // Mashing line-up:          AA A A AAA
  //                             B B B
  // Mashed deck:              AABABABAAA
  //
  // This gets tricky because one of the two blocks may be mashed entirely into the other, or they may only partially intersect, such as with:
  //     AAA A
  //        B BBB
  // |-> AAABABBB
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



  // Returns the [numTimes]-ary composition of [shuffleMethod] with itself.
  def repeatedly(shuffleMethod: Deck => Deck, numTimes: Int): Deck => Deck = v => {if (numTimes > 0) shuffleMethod(repeatedly(shuffleMethod, numTimes-1)(v)) else v}

  // Returns [firstMethod]^a \circ [secondMethod]^b, where a := min([maxTimesForFirstMethod], [numTimes]) and b := max(0, [numTimes] - [maxTimesForFirstMethod]).
  // E.g., repeatedlyChained(firstMethod, secondMethod, 2, 7) == secondMethod^5 \circ firstMethod^2, where "^" denotes the compositional power.
  def repeatedlyChained(firstMethod: Deck => Deck, secondMethod: Deck => Deck, maxTimesForFirstMethod: Int, numTimes: Int): Deck => Deck = {
    if (numTimes <= maxTimesForFirstMethod) {
      repeatedly(firstMethod, numTimes)
    } else {
      v => repeatedly(secondMethod, numTimes - maxTimesForFirstMethod)(repeatedly(firstMethod, maxTimesForFirstMethod)(v))
    }
  }


  val binomialSplitBinomialMash: Deck => Deck = splitMashShuffle(_)
  val sixPile: Deck => Deck = pileShuffle(_, 6)
  val stickyBinomialSplitBinomialMash: Deck => Deck = splitMashShuffle(_, Binomial, Binomial, 0.5)



}
