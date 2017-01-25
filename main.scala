import scala.lms.common._

// Defines a foldLeft() function on Rep[Range]s
trait FoldableRanges extends Dsl {
  implicit def foldableRange(range: Rep[Range]): FoldableRange = new FoldableRange(range)

  class FoldableRange(range: Rep[Range]) {
    def foldLeft(startValue: Rep[Int])(func: (Rep[Int], Rep[Int]) => Rep[Int]): Rep[Int] = {
      var result: Var[Int] = startValue
      for (i <- range.start until range.end) {
        result = func(result, i)
      }
      result
    }
  }
}

// Scala's % operator can return negative numbers;
// the %% operator guarantees nonnegative results.
// See IntOps.scala for a version that works on staged ints.
trait FloorMod {
  def infix_%%(x: Int, y: Int): Int = Math.floorMod(x, y)
}

trait UnstagedRabinKarp extends FloorMod {
  /**
    * Checks whether the pattern 'pattern' is substring of 'text' with Rabin-Karp algorithm.
    * If it maches then the function returns the start index, else returns -1.
    * 
    * http://www.geeksforgeeks.org/searching-for-patterns-set-3-rabin-karp-algorithm/
    *
    * Time - O(n + m)
    * Space - O(1)
    */
  def matchRabinKarp(text: String, pattern: String): Int = {
    val n = text.length
    val m = pattern.length
    val q = 3355439 // a large prime
    val a = 256     // the base of the rolling hash
    val aMax = (1 until m).foldLeft(1)((res, i) => (res * a) %% q) // aMax = a^(m-1) % q

    def hash(string: String, length: Int): Int =
      (0 until length).foldLeft(0)((hash, i) => (hash * a + string(i)) %% q)

    var textHash: Int = hash(text, m)
    val patternHash: Int = hash(pattern, m)

    if (textHash == patternHash)
      return 0

    for (i <- (0 until n - m): Range) {
      val tempHash = (textHash - (aMax * text(i)) %% q) %% q  // subtract old character
      textHash = (tempHash * a + text(i + m)) %% q            // add new character

      if (textHash == patternHash)
        return i + 1
    }

    -1
  }
}

trait StagedRabinKarp extends Dsl with FoldableRanges with FloorMod {
  def matchRabinKarp(text: String, pattern: Rep[String], patternLength: Int): Rep[Int] = {
    val n: Int = text.length
    val m: Int = patternLength
    val q: Int = 3355439 // a large prime
    val a: Int = 256     // the base of the rolling hash
    val aMax: Int = ((1 until m): Range).foldLeft(1)((res, i) => (res * a) %% q) // aMax = a^(m-1) % q

    def hash(string: String, length: Int): Int = 
      ((0 until length): Range).foldLeft(0)((hash, i) => (hash * a + string(i)) %% q)

    def hashStaged(string: Rep[String], length: Int): Rep[Int] = 
      ((0 until length): Rep[Range]).foldLeft(0)((hash, i) => (hash * a + string(i)) %% q)

    var textHash: Int = hash(text, m)
    val patternHash: Rep[Int] = hashStaged(pattern, m)

    if (textHash == patternHash)
      returnL(0)

    for (i <- (0 until n - m): Range) {
      val tempHash = (textHash - (aMax * text(i)) %% q) %% q  // subtract old character
      textHash = (tempHash * a + text(i + m)) %% q            // add new character

      if (textHash == patternHash)
        returnL(i + 1)
    }
    
    -1
  }
}

object Main extends IO with UnstagedRabinKarp {
  val under = "Strings"
  
  // We will search in a pre-defined text of random characters of this length.
  val textSize = 500
  // We will search in the pre-defined text for search patterns of this length.
  val patternSize = 10
  // We will generate this many search patterns.
  val numPatterns = 10000

  // When generating patterns to search for, we generate both random
  // gibberish (which will not appear in the pre-defined text), and random
  // substrings from that text (which will of course be found).
  // This parameter determines the percentage of random gibberish strings.
  val randomStringPercentage = 30

  // When timing the algorithms, we repeat the whole experiment this many times.
  val numTests = 10
  // In each experiment, we match every pattern against the search string.
  // In total, we run both the staged and unstaged algorithm {numTests} * {numPatterns} times.

  val rng = scala.util.Random
  val predefinedSearchText = rng.alphanumeric.take(textSize).mkString

  val snippet = new DslDriver[String,Int] with StagedRabinKarp {
      def snippet(str: Rep[String]) = matchRabinKarp(predefinedSearchText, str, patternSize)
  }

  def matchRabinKarp(pattern: String): Int = {
    snippet.eval(pattern)
  }

  def main(args: Array[String]): Unit = {
    println("Precompiling code snippet...")
    snippet.precompile
    println("Generating code snippet in /out/"+under+"-"+patternSize+".actual.scala")
    exec(patternSize.toString, snippet.code)

    println("Generating random search patterns...")
    val patterns = new Array[String](numPatterns)
    for (i <- 0 until numPatterns) {
      if (rng.nextInt(100) < randomStringPercentage) {
        // Generate random gibberish
        patterns(i) = rng.alphanumeric.take(patternSize).mkString
      }
      else {
        val randomStartIndex = rng.nextInt(predefinedSearchText.length - patternSize + 1)
        // Pick a random fragment from the search string
        patterns(i) = predefinedSearchText.substring(randomStartIndex, randomStartIndex + patternSize)
      }
    }

    var unstagedSearchResults = new Array[Int](0)
    unstagedSearchResults = patterns.map(pattern => matchRabinKarp(predefinedSearchText, pattern))
    var stagedSearchResults = new Array[Int](0)
    stagedSearchResults = patterns.map(pattern => matchRabinKarp(pattern))

    val combinedUnstagedAndStagedResults = unstagedSearchResults zip stagedSearchResults
    val combinedPatternsAndResults = patterns zip combinedUnstagedAndStagedResults

    println("Verifying results...")
    for ((string, (unstagedResult, stagedResult)) <- combinedPatternsAndResults) {
      assert(unstagedResult == stagedResult, "Unstaged and staged results are not the same!")
      println(s"$string -> ($unstagedResult, $stagedResult)")
    }
    println("Staged and unstaged algorithms agree on all inputs!")

    for (test <- 1 to numTests) {
      println(s"\n===== Test $test =====")
      println(s"Running unstaged matcher")
      utils.time {
        unstagedSearchResults = patterns.map(pattern => matchRabinKarp(predefinedSearchText, pattern))
      }
      println("Running staged matcher")
      utils.time {
        stagedSearchResults = patterns.map(pattern => matchRabinKarp(pattern))
      }
    }
  }
}

