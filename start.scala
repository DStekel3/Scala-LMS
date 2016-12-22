import scala.lms.common._

object GettingStarted extends IO {
  val under = "GettingStarted"

  def run() = {
    firstSnippet()
    secondSnippet()
    thirdSnippet()
    fourthSnippet()
    fifthSnippet()    
  }

  def firstSnippet() = {
    println("Generating first code snippet in /out/"+under+"-1.actual.scala")
    val snippet = new DslDriver[Int,Int] {
      def snippet(x: Rep[Int]) = {

        def compute(b: Boolean): Rep[Int] = {
          // the if is executed in the first stage
          if (b) 1 else x
        }
        compute(true)+compute(1==1)
      }
    }
    assert(snippet.eval(0) == 2)
    exec("-1", snippet.code)
  }

  def secondSnippet() = {
    println("Generating second code snippet in /out/"+under+"-2.actual.scala")
    val snippet = new  DslDriver[Int,Int] {
      def snippet(x: Rep[Int]) = {

        def compute(b: Rep[Boolean]): Rep[Int] = {
          // the if is deferred to the second stage
          if (b) 1 else x
        }
        compute(x==1)

      }
    }
    assert(snippet.eval(2) == 2)
    exec("-2", snippet.code)
  }

  def thirdSnippet() = {
    println("Generating third code snippet in /out/"+under+"-3.actual.scala")
    val snippet = new DslDriver[Int,Int] {

      def power(b: Rep[Int], x: Int): Rep[Int] =
        if (x == 0) 1
        else b * power(b, x-1)

      def snippet(b: Rep[Int]) =
        power(b, 3)
    }
    assert(snippet.eval(2) == 8)
    exec("-3", snippet.code)
  }

  def fourthSnippet() = {
    println("Generating fourth code snippet in /out/"+under+"-4.actual.scala")
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = comment("for", verbose = false) {

        for (i <- (0 until 3): Range) {
          println(i)
        }

      }
    }
    exec("-4", snippet.code)
  }

  def fifthSnippet() = {
    println("Generating fifth code snippet in /out/"+under+"-5.actual.scala")
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = comment("for", verbose = false) {

        for (i <- (0 until x): Rep[Range]) {
          println(i)
        }

      }
    }
    exec("-5", snippet.code)
  }
}