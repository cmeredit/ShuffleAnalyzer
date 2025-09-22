import AnalysisMethods.getShuffleLandscape
import IOUtil.saveLandscape
import ShuffleMethods._

object ShuffleTester extends App {

//  val numShuffles: Int = 4
//  val numShuffles: Int = 7
  val numShuffles: Int = 15
//  val deckSize = 177
  val deckSize = 100
//  val numTrials = 1000
  val numTrials = 30_000
//  val numTrials = 1



  val initialDeck: Deck = (0 until deckSize).toVector
  saveLandscape(
    getShuffleLandscape(binomialRiffleShuffle, numShuffles, initialDeck, numTrials),
    filename = "Data/Obj/Sub1/BinomialRiffle30000Trials15Shuffles Corrected.obj",
    xSpacing = 1.0 / (numShuffles.toDouble-1.0),
    zSpacing = 1.0 / (deckSize.toDouble-1.0),
    yScale = 10.0
  )

}
