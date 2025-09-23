# ShuffleAnalyzer
This project provides a framework for defining deck shuffling methods, analyzing those methods, and exporting analysis results.
## Installation and Use
Scala version: 2.13.10
SDK: openjdk-17
External Libraries: None

## Source Files

Decks are modeled as `Vector[Int]` values and the `ShuffleMethods` object provides `Deck` as an alias for `Vector[Int]`.

### Main.scala
Defines an executable object with some examples of using this project.

### ShuffleMethods.scala

A "shuffle method" is a function of type `Deck => Deck`. E.g., perfect shuffling could be defined as a function:
```scala
def perfectShuffle(deck: Deck): Deck = scala.util.Random.shuffle(deck)
```
or as a value:
```scala
val perfectShuffle: Deck => Deck = scala.util.Random.shuffle
```

A few concrete shuffling methods are defined here. Additionally, the method `repeatedly` allows you to compose a shuffling method with itself a specified number of times (e.g., "apply a riffle shuffle 5 times") and `repeatedlyChained` allows you to chain together several different shuffling methods (e.g., "pile shuffle twice and riffle shuffle thereafter").

### AnalysisMethods.scala

Provides helper functions for analyzing shufflign methods. A few important methods are:
- `getNeighborDistanceHeightmap`: The neighbor-distance heuristic measures the effectiveness of a shuffling method according to how far initially-adjacent cards spread from each other upon shuffling. This function computes the probability of each neighbor-distance after some number of applications of the shuffling method. For example, the following code snippet will print the probability that a pair of adjacent cards will have 3 cards between them after 7 applications of a shuffling method, according to 100 simulated trials:
```scala
val shufflingMethod: Deck => Deck = ??? // Your favorite method here
val maxNumShuffles: Int = 10
val initialDeck: Deck = (0 until 100).toVector
val numTrials: Int = 100
val neighborDistanceResults = getNeighborDistanceHeightmap(shufflingMethod, maxNumShuffles, initialDeck, numTrials)

val sevenShufflesNeighborDistanceProbabilities = neighborDistanceResults(6)
println(sevenShufflesNeighborDistanceProbabilities(3))
```
- `getTransitionMatrix`: A transition matrix records the probability of transition between all pairs of states in a system. In the case of shuffling, a transition matrix should record the probability that a card at position `a` in a deck will be shuffled to position `b` for all pairs `(a, b)`. This is exactly what this function computes.
- `getTransitionRows`: Converts a transition matrix to a data vector that is compatible with export methods defined elsewhere.

### IOUtil.scala

Provides I/O utilities. Currently only has functionality for exporting a heightmap to a quad-mesh in .obj format.
