
object Main {
  def main(args: Array[String]): Unit = {
    //println("Running Rabin-Karp string search algorithm...")
    RabinKarp.run()
    println("\nRunning binary tree search algorithm...")
    TreeSearch.run();
    println("\nRunning binary search algorithm...")
    Search.run()
  }
}
