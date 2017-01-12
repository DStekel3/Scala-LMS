import scala.lms.common._

object Search {
  var array = new Array[Int](0)

  def firstSnippet(key: Int) = {
    val snippet = new DslDriver[Int,Int] {
      println("Generating first code snippet in /out/bin_search.scala")
      def binarySearch(key: Rep[Int]): Rep[Int] = {
        def go(lo: Rep[Int], hi: Rep[Int]): Rep[Int] = {
          if (lo > hi)
            -1
          else {
            val mid: Rep[Int] = lo + (hi - lo) / 2
            if(array(mid) == key)
              mid
            else if (array(mid) < key)
              go(mid + 1, hi)
            else
              go(lo, mid - 1)
            /*array(mid) match {
              case mv if (mv == key) => mid
              case mv if (mv <= key) => go(mid + 1, hi)
              case _ => go(lo, mid - 1)
            }*/
          }
        }
      go(0, array.size - 1)
      }

      def snippet(x: Rep[Int]) = {
        binarySearch(x)
      }
    }
    
    println("== " + key + " ==")
      println(snippet.eval(key))
    //exec("search", snippet.code)
  }

  def run() = {

    val r = scala.util.Random
    val z = (for (i <- 1 to 10000) yield r.nextInt(10000)).toArray
    array = quickSort(z)

    System.gc
    firstSnippet(z(6589))
    firstSnippet(0)
    firstSnippet(z(1234))
  }

  /*def execute(key: Int) = {
    println("== " + key + " ==")
    println("")

    println("=== BEFORE SEARCH ===")
    printRam
    Thread.sleep(50)
    println("")
    println(snippet.eval(key))

    println("=== AFTER SEARCH ===")
    Thread.sleep(50)
    printRam

    println("=== AFTER GC ===")
    System.gc
    Thread.sleep(50)
    printRam
    println("")
  }*/

    def quickSort(xs: Array[Int]): Array[Int] = {
        if (xs.length <= 1) xs
        else {
            val pivot = xs(xs.length / 2)
            Array.concat(
                quickSort(xs filter (pivot >)),
                xs filter (pivot ==),
                quickSort(xs filter (pivot <)))
        }
    }

    
}