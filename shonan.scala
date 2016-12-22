import scala.lms.common._

object Shonan extends IO {
  val under = "Shonan"
  val A = scala.Array
  val a =
    A(A(1, 1, 1, 1, 1), // dense
      A(0, 0, 0, 0, 0), // null
      A(0, 0, 1, 0, 0), // sparse
      A(0, 0, 0, 0, 0),
      A(0, 0, 1, 0, 1))

  def run() = {
    staticImplementation()
    staticTestSnippet()
    dynamicTestSnippet()
    firstSnippet()
    secondSnippet()
  }

  def staticImplementation() = {
    println("Calculating vector-matrix product using static implementation...")
    def matrix_vector_prod(a: Array[Array[Int]], v: Array[Int]) = {
      val n = a.length
      val v1 = new Array[Int](n)

      for (i <- (0 until n)) {
        for (j <- (0 until n)) {
          v1(i) = v1(i) + a(i)(j) * v(j)
        }
      }
      v1
    }

    // let's run it on some static input:
    val v = A(1,1,1,1,1)
    val v1 = matrix_vector_prod(a, v)
    val result = v1.mkString(",")
    println(result)
  }

  def staticTestSnippet() = {
    println("Generating static test code snippet in /out/"+under+"-static_test.actual.scala")
    val snippet = new DslDriver[Array[Int],Array[Int]] {
      def snippet(v: Rep[Array[Int]]) = {

        val x = 100 - 5

        if (x > 10) {
          println("hello")
        }

        v
      }
    }
    exec("-static_test", snippet.code)
  }

  def dynamicTestSnippet() = {
    println("Generating static test code snippet in /out/"+under+"-dynamic_test.actual.scala")
    val snippet = new DslDriver[Array[Int],Array[Int]] {
      def snippet(v: Rep[Array[Int]]) = {

        val x = 100 - v.length

        if (x > 10) {
          println("hello")
        }

        v
      }
    }
    exec("-dynamic_test", snippet.code)
  }

  def firstSnippet() = {
    println("Generating first code snippet in /out/"+under+"-1.actual.scala")
    val snippet = new DslDriver[Array[Int],Array[Int]] {
      def snippet(v: Rep[Array[Int]]) = {

        def matrix_vector_prod(a0: Array[Array[Int]], v: Rep[Array[Int]]) = {
          val n = a0.length
          val a = staticData(a0)
          val v1 = NewArray[Int](n)

          for (i <- (0 until n):Range) {
            val sparse = a0(i).count(_ != 0) < 3
            if (sparse) {
              for (j <- (0 until n):Range) {
                v1(i) = v1(i) + a(i).apply(j) * v(j)
              }
            } else {
              for (j <- (0 until n):Rep[Range]) {
                v1(i) = v1(i) + a(i).apply(j) * v(j)
              }
            }
          }
          v1
        }

        val v1 = matrix_vector_prod(a, v)
        v1
      }
    }
    assert(snippet.eval(A(1,1,1,1,1)).deep == A(5,0,1,0,2).deep)
    exec("-1", snippet.code)
  }

  def secondSnippet() = {
    println("Generating second code snippet in /out/"+under+"-2.actual.scala")
    val snippet = new DslDriver[Array[Int],Array[Int]] {
      def snippet(v: Rep[Array[Int]]) = {

        def unrollIf(c:Boolean,r: Range) = new {
          def foreach(f: Rep[Int] => Rep[Unit]) = {
            if (c) for (j <- (r.start until r.end):Range)      f(j)
            else   for (j <- (r.start until r.end):Rep[Range]) f(j)
          }
        }

        def matrix_vector_prod(a0: Array[Array[Int]], v: Rep[Array[Int]]) = {
          val n = a0.length
          val a = staticData(a0)
          val v1 = NewArray[Int](n)

          for (i <- (0 until n):Range) {
            val sparse = a0(i).count(_ != 0) < 3
            for (j <- unrollIf(sparse, 0 until n)) {
              v1(i) = v1(i) + a(i).apply(j) * v(j)
            }
          }
          v1
        }

        val v1 = matrix_vector_prod(a, v)
        v1
      }
    }
    assert(snippet.eval(A(1,1,1,1,1)).deep == A(5,0,1,0,2).deep)
    exec("-2", snippet.code)
  }
}