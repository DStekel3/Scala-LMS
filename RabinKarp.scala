import scala.lms.common._

/**
  * This code is based on the Scalacaster project, https://github.com/vkostyukov/scalacaster
  * which is written by Vladimir Kostyukov, http://vkostyukov.ru
  *
  * We have taken the implementation of the Rabin-Karp string searching algorithm that can be found there.
  * This algorithm was implemented in a single function:
  *     matchRabinKarp(text: String, pattern: String): Int
  * See https://en.wikipedia.org/wiki/Rabin-Karp_algorithm for an explanation of the algorithm.
  * We fixed the code so it worked correctly, modified it so it could be staged,
  * and then implemented a staged version of the algorithm.
  * You can find the generated code in the "/out" folder.
  * 
  * The experiment parameters are defined directly below.
  * After that comes the run() function that performs the experiment.
  * The unstaged and staged implementations of the Rabin-Karp algorithm are defined below that.
  */

object RabinKarp extends IO with UnstagedRabinKarp {
  val under = "RabinKarp"
  
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
  // The pre-defined text consists of {textSize} random alphanumeric characters.
  val predefinedSearchText = rng.alphanumeric.take(textSize).mkString

  val snippet = new DslDriver[String,Int] with StagedRabinKarp {
      def snippet(str: Rep[String]) = matchRabinKarp(predefinedSearchText, str, patternSize)
  }

  def matchRabinKarp(pattern: String): Int = {
    snippet.eval(pattern)
  }

  def run(): Unit = {
    println("Precompiling code snippet...")
    snippet.precompile
    println("Generating code snippet in /out/"+under+"-"+textSize+"-"+patternSize+".actual.scala")
    exec("-"+textSize.toString+"-"+patternSize.toString, snippet.code)

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

    // Run both the staged and unstaged functions against all inputs once.
    // This gives the JVM the chance to do things like optimizing frequently
    // called functions before we actually start timing things.
    var unstagedSearchResults = new Array[Int](0)
    unstagedSearchResults = patterns.map(pattern => matchRabinKarp(predefinedSearchText, pattern))
    var stagedSearchResults = new Array[Int](0)
    stagedSearchResults = patterns.map(pattern => matchRabinKarp(pattern))

    val combinedUnstagedAndStagedResults = unstagedSearchResults zip stagedSearchResults

    println("Verifying results...")
    for ((unstagedResult, stagedResult) <- combinedUnstagedAndStagedResults) {
      assert(unstagedResult == stagedResult, "Unstaged and staged results are not the same!")
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

/**
  * The original code simply used Scala's modulo (%) operator. However,
  * this doesn't work: the modulo operator in Scala performs truncated division.
  * This yields incorrect results for negative numbers:
  *     3 % 8 = 3
  *    -3 % 8 = -3
  *
  * We have added a new operator, %%, that relies on Scala's Math.floorMod() function.
  * This works correctly:
  *     3 %% 8 = 3
  *    -3 %% 8 = 5
  *
  * The version of %% that works on regular Ints is defined here.
  * See IntOps.scala for a version that works on Rep[Int]s.
  */
trait FloorMod {
  def infix_%%(x: Int, y: Int): Int = Math.floorMod(x, y)
}

/**
  * The original code used the .foldLeft() function on ranges, like this:
  *     (1 until m).foldLeft(1)((res, i) => (res * a) %% q)
  * Unfortunately, the foldLeft() function was not pre-defined for Rep[Range]s.
  * We have implemented this here.
  */
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

/**
  * This is the unstaged Rabin-Karp algorithm.
  * Note that it has been modified to use our new %% operator.
  * Also, the original version of the algorithm was recursive
  * (this can be seen in the Scalacaster GitHub repository).
  * When we wrote the staged version of this function, we found
  * that the staged recursive function lead to excessive nesting
  * of if-expressions in the generated code.
  * (n levels of nesting for an input of length n, which leads to generator crashes when n > ~50)
  * We have therefore re-written the algorithm to use a for-loop.
  */
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

/**
  * This is the staged Rabin-Karp algorithm.
  * The text to be searched is known at staging time; the
  * pattern for which to search will only be known at run-time.
  *
  * Note that in order to generate a properly optimized version of the function, we
  * do not need to know the search pattern, but we do need to know the length of the pattern.
  * We have therefore added an additional patternLength parameter, which will have to be
  * provided at staging time. So, if the provided length is 10, for example, the generated
  * function will only be able to match patterns of length 10.
  *
  * We imagine that users of this function will typically want to search for
  * short pieces of text in long texts; one could generate a number of different versions of
  * this function, one for each desired pattern length, and store them in a Map.
  * When a pattern would have to be matched, the function with the corresponding pattern length
  * could then be retrieved from the map; or, if a generated function is not found, it could
  * be generated at that moment (this would of course take a while).
  * For our experiments, we have simply picked one pattern length, generated one corresponding
  * function, and only generated patterns of that specific length.
  *
  * We have added Rep type annotations to the original function.
  * We ran into type errors, because not all operations we were using were defined for staged types.
  * - The FoldableRanges trait defines a foldLeft() function for Rep[Range]s
  *   -> see above
  * - We have defined the %% operator for Rep[Int]s in terms of IR nodes/generated code
  *   -> see IntOps.scala
  * - We perform arithmetic operations on a mix of Chars and Ints. In regular Scala, Chars
  *     will be implicitly converted to Ints where needed; this doesn't happen for Rep[Char]s.
  *     We have added an implicit .toInt function on Rep[Char]s in terms of IR nodes/generated code
  *   -> see CharOps.scala
  *
  * Also note that we could simply return from inside the unstaged matchRabinKarp() function.
  * Scala LMS doesn't allow you to return from a staged function.
  * A partially working implementation of return is provided as a function called returnL().
  * Apparently, this is not always guaranteed to work.
  * Fortunately, it works fine for our particular use case, so we use returnL() here.
  * (In general, instead of writing an imperative-style for-loop with return statements,
  * it is good Scala practice to write recursive functions, but that is precisely what we
  * were trying to avoid because a recursive implementation leads to too much nesting
  * in the generated code files. This for-loop-implementation generates code with an unrolled loop;
  * a flat, non-nested structure. It is interesting to node that in order to get a properly
  * optimized function, one sometimes has to forsake the idiomatic code style.)
  */
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
