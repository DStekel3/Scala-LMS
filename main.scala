import scala.lms.common._

object Main extends IO {
  val under = "Strings"

  
  val predefinedPattern = "Scala"
  //val predefinedString = "Since Fender Stratocaster is a classic guitar, Scalacaster is about classic algorithms and data structures in Scala. Scalacaster includes loads of widely used implementation techniques and approaches, which have been developed by best programmers and enthusiasts of functional programming. Studying purely functional data structures is always fun and challenge for researchers, since data structures in a functional setting are much elegant and smarter than in an imperative setting."

  val predefinedString = "Banana"

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
      // Scala's % operator can return negative numbers;
      // the floorMod() function guarantees nonnegative results
      import Math.{floorMod => mod}

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
          val tempHash = mod(textHash - mod(aMax * text(i), q), q) // subtract old character
          val newHash = mod(tempHash * a + text(i + m), q)         // add new character
          
          //println(s"${i + 1}: ${newHash}")
          loop(newHash, patternHash, i + 1)
        }
      }
      //println(s">: ${hash(pattern, m)}")
      //println(s"0: ${hash(text, m)}")
      loop(hash(text, m), hash(pattern, m), 0)
    }

    var snippet = new DslDriver[String,Int] {
      def snippet(str: Rep[String]) = {

        def matchRabinKarpStaged(text: Rep[String], pattern: String): Rep[Int] = {
          // Scala's % operator can return negative numbers;
          // the mod() function guarantees nonnegative results
          import Math.{floorMod => mod}

          def modStaged(x: Rep[Int], y: Int): Rep[Int] = {
            val temp = x % y
            val res = if (temp < 0) {
              temp + y
            }
            else {
              temp
            }
            res
          }

          val n: Rep[Int] = text.length
          val m: Int = pattern.length
          val q: Int = 3355439 // a large prime
          val a: Int = 256     // the base of the rolling hash
          val aMax: Int = {    // aMax = a^(m-1) % q 
            var res: Int = 1
            for (i <- (1 until m): Range) {
              res = mod(res * a, q)
            }
            res
          }

          def hash(string: String, length: Int): Int = {
            var hash: Int = 0
            for (i <- (0 until length): Range) {
              hash = mod(hash * a + string.charAt(i), q)
            }
            hash
          }

          def hashStaged(string: Rep[String], length: Int): Rep[Int] = {
            var hash: Rep[Int] = unit(0)
            for (i <- (0 until length): Range) {
              hash = modStaged(hash * a + string.charAt(i), q)
            }
            hash
          }

          def loop(patternHash: Int): Rep[((Int, Int)) => Int] = { (i: Rep[Int], textHash: Rep[Int]) =>
            if (textHash == patternHash) i
            else if (i == n - m) -1
            else {
              val tempHash = modStaged(textHash - modStaged(aMax * text.charAt(i), q), q) // subtract old character
              val newHash = modStaged(tempHash * a + text.charAt(i + m), q)               // add new character
              
              //println((i + 1) + ": " + newHash)
              loop(patternHash)(i + 1, newHash)
            }
          }
          //println(">: " + hash(pattern, m))
          //println("0: " + hashStaged(text, m))

          loop(hash(pattern, m))(0, hashStaged(text, m))
        }
        matchRabinKarpStaged(str, predefinedPattern)
      }
    }

    def matchRabinKarpStaged(text: String): Int = {
      snippet.eval(text)
    }

    println("Generating code snippet in /out/"+under+"-1.actual.scala")
    exec("-1", snippet.code)
    
    val patterns = List(
      "The pattern is Scala",
      "scalasCALAScala",
      "This string does not contain the pattern",
      "This one, however, does: Scala!"
    )
    
    val unstagedSearchResults = patterns.map(string => matchRabinKarp(string, predefinedPattern))
    val patternSearchResults = patterns.map(string => matchRabinKarpStaged(string))
    
    val combinedUnstagedSearchResults = patterns zip unstagedSearchResults
    val combinedPatternSearchResults = patterns zip patternSearchResults

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

