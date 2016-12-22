object Main {
  
  def main(args: Array[String]): Unit = {

    // The Getting Started tutorial - see start.scala
    // https://scala-lms.github.io/tutorials/start.html 
    // println("\n::: Getting started :::")
    // GettingStarted.run()

    // // The Shonan HMM Challenge tutorial - see shonan.scala
    // // https://scala-lms.github.io/tutorials/shonan.html
    // println("\n::: Shonan HMM Challenge :::")
    // Shonan.run()

    // // The Regular Expressions tutorial - see regex.scala
    // // https://scala-lms.github.io/tutorials/regex.html
    // println("\n::: Regular Expressions :::")
    // Regex.run()

    // // The Automata-Based Regex Matcher tutorial - see automata.scala
    // // https://scala-lms.github.io/tutorials/automata.html
    // println("\n::: Automata-Based Regex Matcher :::")
    // Automata.run()

    // // The Sliding Stencil tutorial - see stencil.scala
    // // https://scala-lms.github.io/tutorials/stencil.html
    // println("\n::: Sliding Stencil :::")
    // Stencil.run()

    println("\n::: Begin searching tree :::")
    TreeSearch.run();

    /* Running this test along with the others may result in Java GC/out of memory errors
     * If it is run by itself, with the other tests disabled, it works fine */
    // The Ackermann's Function tutorial - see ack.scala
    // https://scala-lms.github.io/tutorials/ack.html
    // println("\n::: Ackermann's Function :::")
    // Ackermann.run()

    /* The following tutorials are not included:

     * The SQL Engine tutorial
     * https://scala-lms.github.io/tutorials/query.html
     * Far more code than the other tutorials, spread over multiple files

     * The Fast Fourier tutorial
     * https://scala-lms.github.io/tutorials/fft.html
     * The code, as it appears on the site, was sloppily copied from a paper
     * It is completely broken

     */
  }
}