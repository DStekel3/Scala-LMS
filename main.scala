import scala.lms.common._

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

trait UnstagedRabinKarp {
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
    // Scala's % operator can return negative numbers;
    // the floorMod() function (imported as mod()) guarantees nonnegative results
    import Math.{floorMod => mod}

    val n = text.length
    val m = pattern.length
    val q = 3355439 // a large prime
    val a = 256     // the base of the rolling hash
    val aMax = (1 until m).foldLeft(1)((res, i) => mod(res * a, q)) // aMax = a^(m-1) % q

    def hash(string: String, length: Int): Int =
      (0 until length).foldLeft(0)((hash, i) => mod(hash * a + string(i), q))

    def loop(textHash: Int, patternHash: Int, i: Int): Int = {
      if (textHash == patternHash) i
      else if (i == n - m) -1
      else {
        val tempHash = mod(textHash - mod(aMax * text(i), q), q) // subtract old character
        val newHash = mod(tempHash * a + text(i + m), q)         // add new character
        loop(newHash, patternHash, i + 1)
      }
    }
    loop(hash(text, m), hash(pattern, m), 0)
  }
}

trait StagedRabinKarp extends Dsl with FoldableRanges {
  // Scala's % operator can return negative numbers;
  // the mod() functions guarantee nonnegative results
  def mod(x: Int, y: Int): Int = Math.floorMod(x, y)

  def mod(x: Rep[Int], y: Int): Rep[Int] = {
    var res: Var[Int] = x % y
    if (res < 0) {
      res += y
    }
    res
  }

  def matchRabinKarp(text: Rep[String], pattern: String): Rep[Int] = {
    val n: Rep[Int] = text.length
    val m: Int = pattern.length
    val q: Int = 3355439 // a large prime
    val a: Int = 256     // the base of the rolling hash
    val aMax: Int = ((1 until m): Range).foldLeft(1)((res, i) => mod(res * a, q)) // aMax = a^(m-1) % q

    def hash(string: String, length: Int): Int = 
      ((0 until length): Range).foldLeft(0)((hash, i) => mod(hash * a + string(i), q))

    def hashStaged(string: Rep[String], length: Int): Rep[Int] = 
      ((0 until length): Rep[Range]).foldLeft(0)((hash, i) => mod(hash * a + string(i), q))

    def loop(patternHash: Int): Rep[((Int, Int)) => Int] = { (i: Rep[Int], textHash: Rep[Int]) =>
      if (textHash == patternHash) i
      else if (i == n - m) -1
      else {
        val tempHash = mod(textHash - mod(aMax * text.charAt(i), q), q) // subtract old character
        val newHash = mod(tempHash * a + text.charAt(i + m), q)         // add new character
        loop(patternHash)(i + 1, newHash)
      }
    }
    loop(hash(pattern, m))(0, hashStaged(text, m))
  }
}

object Main extends IO with UnstagedRabinKarp {
  val under = "Strings"
  
  val predefinedPattern = "Scala"
  val predefinedString = "Since Fender Stratocaster is a classic guitar, Scalacaster is about classic algorithms and data structures in Scala. Scalacaster includes loads of widely used implementation techniques and approaches, which have been developed by best programmers and enthusiasts of functional programming. Studying purely functional data structures is always fun and challenge for researchers, since data structures in a functional setting are much elegant and smarter than in an imperative setting."
  
  val snippet: DslDriver[String,Int] = new DslDriver[String,Int] with StagedRabinKarp {
    def snippet(str: Rep[String]) = matchRabinKarp(str, predefinedPattern)
  }

  def matchRabinKarp(text: String): Int = {
    snippet.eval(text)
  }

  def main(args: Array[String]): Unit = {
    println("Generating code snippet in /out/"+under+"-1.actual.scala")
    exec("-1", snippet.code)
    
    val texts = List(
      "The pattern is Scala",
      "scalasCALAScala",
      "This string does not contain the pattern",
      "This one, however, does: Scala!"
    )
    
    val unstagedSearchResults = texts.map(string => matchRabinKarp(string, predefinedPattern))
    val patternSearchResults = texts.map(string => matchRabinKarp(string))
    
    val combinedUnstagedSearchResults = texts zip unstagedSearchResults
    val combinedPatternSearchResults = texts zip patternSearchResults

    println("Unstaged:")
    for ((string, result) <- combinedUnstagedSearchResults) {
      println(s"$string -> $result")
    }
    println("\nStaged:")
    for ((string, result) <- combinedPatternSearchResults) {
      println(s"$string -> $result")
    }
  }
}

