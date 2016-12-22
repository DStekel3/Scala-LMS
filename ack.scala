import scala.lms.common._

trait AckermannFunction extends Dsl {
  def a(m: Int): Rep[Int => Int] = fun { (n: Rep[Int]) =>
    generate_comment("ack_"+m) // to navigate the generated code
    if (m==0) n+1
    else if (n==0) a(m-1)(1)
    else a(m-1)(a(m)(n-1))
  }
}

object Ackermann extends IO {
  val under = "Ack"
  def specialize(m: Int): DslDriver[Int,Int] = new DslDriver[Int,Int] with AckermannFunction {
    def snippet(n: Rep[Int]): Rep[Int] = a(m)(n)
  }

  def run() = {
    println("Generating code snippet for m=0 in /out/"+under+"-m0.actual.scala")
    val ack0 = specialize(0)
    assert(ack0.eval(0) == 1)
    assert(ack0.eval(1) == 2)
    assert(ack0.eval(2) == 3)
    assert(ack0.eval(3) == 4)
    exec("-m0", ack0.code)

    println("Generating code snippet for m=1 in /out/"+under+"-m1.actual.scala")
    val ack1 = specialize(1)
    assert(ack1.eval(0) == 2)
    assert(ack1.eval(1) == 3)
    assert(ack1.eval(2) == 4)
    assert(ack1.eval(3) == 5)
    exec("-m1", ack1.code)

    println("Generating code snippet for m=2 in /out/"+under+"-m2.actual.scala")
    val ack2 = specialize(2)
    assert(ack2.eval(0) == 3)
    assert(ack2.eval(1) == 5)
    assert(ack2.eval(2) == 7)
    assert(ack2.eval(3) == 9)
    exec("-m2", ack2.code)

    println("Generating code snippet for m=3 in /out/"+under+"-m3.actual.scala")
    val ack3 = specialize(3)
    assert(ack3.eval(0) == 5)
    assert(ack3.eval(1) == 13)
    assert(ack3.eval(2) == 29)
    assert(ack3.eval(3) == 61)
    exec("-m3", ack3.code)
  }
}