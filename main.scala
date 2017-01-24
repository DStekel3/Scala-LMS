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
  
  val predefinedPatternLength = 10
  val predefinedString = "Since Fender Stratocaster is a classic guitar, Scalacaster is about classic algorithms and data structures in Scala. Scalacaster includes loads of widely used implementation techniques and approaches, which have been developed by best programmers and enthusiasts of functional programming. Studying purely functional data structures is always fun and challenge for researchers, since data structures in a functional setting are much elegant and smarter than in an imperative setting."

  val snippet: DslDriver[String,Int] = new DslDriver[String,Int] with StagedRabinKarp {
    def snippet(str: Rep[String]) = matchRabinKarp(predefinedString, str, predefinedPatternLength)
  }

  def matchRabinKarp(text: String): Int = {
    snippet.eval(text)
  }

  def main(args: Array[String]): Unit = {
    println("Generating code snippet in /out/"+under+"-1.actual.scala")
    exec("-1", snippet.code)
    
    val patterns = List(
      "Since Fend",
      "Fender Str",
      "Stratocast",
      "Scala lang",
      "functional",
      "imperative",
      "e setting."
    )

    patterns.foreach { pattern: String => assert(pattern.length == predefinedPatternLength,
      s"""Search pattern \"$pattern\" has length ${pattern.length}; it must be $predefinedPatternLength!""") }

    println("Precompiling generated code")
    snippet.precompile
    
    println("Running unstaged matcher")
    val unstagedSearchResults = patterns.map(pattern => matchRabinKarp(predefinedString, pattern))
    println("Running staged matcher")
    val stagedSearchResults = patterns.map(pattern => matchRabinKarp(pattern))
    
    val combinedUnstagedSearchResults = patterns zip unstagedSearchResults
    val combinedStagedSearchResults = patterns zip stagedSearchResults

    println("Unstaged:")
    for ((string, result) <- combinedUnstagedSearchResults) {
      println(s"$string -> $result")
    }
    println("\nStaged:")
    for ((string, result) <- combinedStagedSearchResults) {
      println(s"$string -> $result")
    }
  }
}

