import scala.lms.common._
import Math.{floorMod => mod}

object Main extends IO {
  val under = "Strings"

  def main(args: Array[String]): Unit = {
    /**
     * Checks whether the pattern 'p' is substring of 's' with Rabin-Karp algorithm.
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
      val aMax = {    // aMax = a^(m-1) % q 
        var res = 1
        for (i <- 1 until m) {
          res = mod(res * a, q)
        }
        res
      }

      def hash(string: String, length: Int): Int = {
        var hash = 0
        for (i <- 0 until length) {
          hash = mod(hash * a + string(i), q)
        }
        hash
      }

      def loop(textHash: Int, patternHash: Int, i: Int): Int = {
        if (textHash == patternHash) i
        else if (i == n - m) -1
        else {
          val tempHash = mod(textHash - mod(aMax * text(i), q), q)     // subtract old character
          loop(mod(tempHash * a + text(i + m), q), patternHash, i + 1) // add new character and loop
        }
      }
      loop(hash(text, m), hash(pattern, m), 0)
    }

    var snippet = new DslDriver[String,Int] {
      def snippet(str: Rep[String]) = {

        def matchRabinKarpStaged(text: Rep[String], pattern: String): Rep[Int] = {
          val n:Rep[Int] = text.length
          val m:Int = pattern.length
          val q:Int = 3355439 // a large prime
          val a:Int = 256     // the base of the rolling hash
          val aMax:Int = {    // aMax = a^(m-1) % q 
            var res: Int = 1
            for (i <- 1 until m) {
              res = mod(res * a, q)
            }
            res
          }
          def hash(string: String, length: Int): Int = {
            var hash:Int = 0
            for (i <- 0 until length) {
              hash = mod(hash * a /*+ string.charAt(i)*/, q)
            }
            hash
          }

          // def hashStaged(string: Rep[String], length: Int): Rep[Int] = {
          //   var hash = 0
          //   for (i <- 0 until length) {
          //     hash = mod(hash * a + string.charAt(i), q)
          //   }
          //   hash
          // }

          def loop(textHash: Rep[Int], patternHash: Int, i: Int): Rep[Int] = {
            if (textHash == patternHash) i
            else if (i == n - m) -1
            else {
              val tempHash = mod(textHash - mod(aMax /* * text.charAt(i)*/, q), q)     // subtract old character
              loop(mod(tempHash * a /*+ text.charAt(i + m)*/, q), patternHash, i + 1) // add new character and loop
            }
          }
          loop(/*hashPattern(text, m)*/0, hash(pattern, m), 0)
        }
        matchRabinKarpStaged(str, predefinedPattern)
      }
    }

    val predefinedPattern = "Scala"
    val predefinedString = "Since Fender Stratocaster is a classic guitar, Scalacaster is about classic algorithms and data structures in Scala. Scalacaster includes loads of widely used implementation techniques and approaches, which have been developed by best programmers and enthusiasts of functional programming. Studying purely functional data structures is always fun and challenge for researchers, since data structures in a functional setting are much elegant and smarter than in an imperative setting."

    val patterns = List(
      "Scala",
      "Java",
      "classic guitar",
      "AbstractSingletonProxyFactoryBean",
      "Since",
      "imperative setting.",
      "perative setting!"
    )

    val patternSearchResults = patterns.map(string => matchRabinKarp(predefinedString, string))
    val combinedPatternSearchResults = patterns zip patternSearchResults
    for ((string, result) <- combinedPatternSearchResults) {
      println(s"$string -> $result")
    }
  }
}

