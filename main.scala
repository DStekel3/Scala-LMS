import scala.lms.common._

object Main extends IO {
  val under = "Example"
  
  def main(args: Array[String]): Unit = {
    val snippet = new DslDriver[String, Unit] {
      def snippet(str: Rep[String]) = {
        println("Hello, " + str)
      }
    }
    println("Generating code snippet in /out/" + under +"-hello.actual.scala")
    exec("-hello", snippet.code)
    
    println("Running code snippet")
    snippet.eval("world!")
  }
}