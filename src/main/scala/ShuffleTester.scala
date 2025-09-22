import AnalysisMethods.getNeighborDistanceHeightmap
import IOUtil.saveHeightmap
import ShuffleMethods._

object ShuffleTester extends App {

  // Saves a "neighbor distance heightmap" for the binomial riffle shuffle (i.e., GSR shuffle).
  // This heightmap is a function H: {0, ..., [deckSize] - 2} x {1, ..., [numShuffles]} defined as follows.
  // Fix d \in {0, ..., [deckSize] - 2} and s \in {1, ..., [numShuffles]}.
  // Let S denote the s-fold GSR shuffle (i.e., S is a random variable on the symmetric group on [deckSize] letters). Let C be uniformly distributed over {0, ..., [deckSize] - 2}.
  // Then H(d, s) = P(|S(C) - S(C + 1)| = d)
  // I.e., H(d, s) is the probability that a pair of neighboring cards in the initial deck will have d cards between them after s applications of the GSR shuffle.
  val numShuffles: Int = 15
  val deckSize = 100
  val numTrials = 30_000
  val initialDeck: Deck = (0 until deckSize).toVector
  saveHeightmap(
    getNeighborDistanceHeightmap(binomialRiffleShuffle, numShuffles, initialDeck, numTrials),
    filename = "Data/Obj/BinomialRiffle30000Trials15Shuffles Corrected.obj",
    xSpacing = 1.0 / (numShuffles.toDouble-1.0),
    zSpacing = 1.0 / (deckSize.toDouble-1.0),
    yScale = 10.0
  )

}
