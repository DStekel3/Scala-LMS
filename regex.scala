import scala.lms.common._

trait RegexpMatcher {

  /* search for regexp anywhere in text */
  def matchsearch(regexp: String, text: String): Boolean = {
    if (regexp(0) == '^')
      matchhere(regexp, 1, text, 0)
    else {
      var start = -1
      var found = false
      while (!found && start < text.length) {
        start += 1
        found = matchhere(regexp, 0, text, start)
      }
      found
    }
  }


  /* search for restart of regexp at start of text */
  def matchhere(regexp: String, restart: Int, text: String, start: Int): Boolean = {
    if (restart==regexp.length)
      true
    else if (regexp(restart)=='$' && restart+1==regexp.length)
      start==text.length
    else if (restart+1 < regexp.length && regexp(restart+1)=='*')
      matchstar(regexp(restart), regexp, restart+2, text, start)
    else if (start < text.length && matchchar(regexp(restart), text(start)))
      matchhere(regexp, restart+1, text, start+1)
    else false
  }

  /* search for c* followed by restart of regexp at start of text */
  def matchstar(c: Char, regexp: String, restart: Int, text: String, start: Int): Boolean = {
    var sstart = start
    var found = matchhere(regexp, restart, text, sstart)
    var failed = false
    while (!failed && !found && sstart < text.length) {
      failed = !matchchar(c, text(sstart))
      sstart += 1
      found = matchhere(regexp, restart, text, sstart)
    }
    !failed && found
  }

  def matchchar(c: Char, t: Char): Boolean = {
    c == '.' || c == t
  }
}

trait StagedRegexpMatcher extends Dsl {

  /* search for regexp anywhere in text */
  def matchsearch(regexp: String, text: Rep[String]): Rep[Boolean] = {
    if (regexp(0) == '^')
      matchhere(regexp, 1, text, 0)
    else {
      var start = -1
      var found = false
      while (!found && start < text.length) {
        start += 1
        found = matchhere(regexp, 0, text, start)
      }
      found
    }
  }

  /* search for restart of regexp at start of text */
  def matchhere(regexp: String, restart: Int, text: Rep[String], start: Rep[Int]): Rep[Boolean] = {
    if (restart==regexp.length)
      true
    else if (regexp(restart)=='$' && restart+1==regexp.length)
      start==text.length
    else if (restart+1 < regexp.length && regexp(restart+1)=='*')
      matchstar(regexp(restart), regexp, restart+2, text, start)
    else if (start < text.length && matchchar(regexp(restart), text(start)))
      matchhere(regexp, restart+1, text, start+1)
    else false
  }

  /* search for c* followed by restart of regexp at start of text */
  def matchstar(c: Char, regexp: String, restart: Int, text: Rep[String], start: Rep[Int]): Rep[Boolean] = {
    var sstart = start
    var found = matchhere(regexp, restart, text, sstart)
    var failed = false
    while (!failed && !found && sstart < text.length) {
      failed = !matchchar(c, text(sstart))
      sstart += 1
      found = matchhere(regexp, restart, text, sstart)
    }
    !failed && found
  }

  def matchchar(c: Char, t: Rep[Char]): Rep[Boolean] = {
    c == '.' || c == t
  }
}

object Regex extends IO with RegexpMatcher {
  val under = "Regex"
  
  def run() = {
    testUnstagedMatcher()
    testStagedMatcher()
  }

  def testmatchUnstaged(regexp: String, text: String, expected: Boolean) {
    assert(matchsearch(regexp, text) == expected)
  }

  def testUnstagedMatcher() = {
    println("Testing unstaged regex matcher using assertions...")
    testmatchUnstaged("^hello$", "hello", true)
    testmatchUnstaged("^hello$", "hell", false)
    testmatchUnstaged("hell", "hello", true);
    testmatchUnstaged("hell", "hell", true);
    testmatchUnstaged("hel*", "he", true);
    testmatchUnstaged("hel*$", "hello", false);
    testmatchUnstaged("hel*", "yo hello", true);
    testmatchUnstaged("ab", "hello ab hello", true);
    testmatchUnstaged("^ab", "hello ab hello", false);
    testmatchUnstaged("a*b", "hello aab hello", true);
    testmatchUnstaged("^ab*", "abcd", true);
    testmatchUnstaged("^ab*", "a", true);
    testmatchUnstaged("^ab*", "ac", true);
    testmatchUnstaged("^ab*", "bac", false);
    testmatchUnstaged("^ab*$", "ac", false);
    println("All regex tests succeeded.")
  }

  var m = Map.empty[String, DslDriver[String,Boolean]]
  def cache(k: String, b: => DslDriver[String,Boolean]): DslDriver[String,Boolean] = {
    m.get(k) match {
      case Some(v) => v
      case None =>
        m = m.updated(k, b)
        m(k)
    }
  }

  def testmatchStaged(regexp: String, text: String, expected: Boolean) {
    val snippet = cache(regexp,
      new DslDriver[String,Boolean] with StagedRegexpMatcher {
        def snippet(x: Rep[String]) = matchsearch(regexp, x)
    })
    assert(snippet.eval(text) == expected)
    val fileNamePrefix = "-"+regexp.replace("^", "_b").replace("*", "_s").replace("$", "_e")
    println("Generating code snippet in /out/"+under+fileNamePrefix+".actual.scala")
    exec(fileNamePrefix, snippet.code)
  }

  def testStagedMatcher() = {
    println("Testing staged regex matcher using assertions...")
    testmatchStaged("^hello$", "hello", true)
    testmatchStaged("^hello$", "hell", false)
    testmatchStaged("hell", "hello", true)
    testmatchStaged("hell", "hell", true)
    testmatchStaged("hel*", "he", true)
    testmatchStaged("hel*$", "hello", false)
    testmatchStaged("hel*", "yo hello", true)
    testmatchStaged("ab", "hello ab hello", true)
    testmatchStaged("^ab", "hello ab hello", false)
    testmatchStaged("a*b", "hello aab hello", true)
    testmatchStaged("^ab*", "abcd", true)
    testmatchStaged("^ab*", "a", true)
    testmatchStaged("^ab*", "ac", true)
    testmatchStaged("^ab*", "bac", false)
    testmatchStaged("^ab*$", "ac", false)
    println("All regex tests succeeded.")
  }
}