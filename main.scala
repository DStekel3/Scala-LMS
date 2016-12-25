import scala.lms.common._

import scala.reflect.SourceContext
import java.io.PrintWriter

trait Power1 { this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double] = 
    if (x == 0) 1.0 else b * power(b, x - 1)
}

trait Power2 { this: Arith =>
  def power(b: Rep[Double], x: Int)(implicit pos: SourceContext): Rep[Double] = {
    if (x == 0) 1.0
    else if ((x&1) == 0) { val y = power(b, x/2); y * y }
    else b * power(b, x - 1)
  }
}

trait ScalaGenFlat extends ScalaGenBase {
   import IR._
   type Block[+T] = Exp[T]
   def getBlockResultFull[T](x: Block[T]): Exp[T] = x
   def reifyBlock[T:Typ](x: =>Exp[T]): Block[T] = x
   def traverseBlock[A](block: Block[A]): Unit = {
     buildScheduleForResult(block) foreach traverseStm
   }
}

trait BaseStr extends Base {
  type Rep[+T] = String
  //todo added this to provide required unit implicit conversion
  implicit def unit[T:Typ](x: T): Rep[T] = x.toString

  case class Typ[T](m: Manifest[T])

  def typ[T:Typ]: Typ[T] = implicitly[Typ[T]]

  implicit def unitTyp: Typ[Unit] = Typ(implicitly)
  implicit def nullTyp: Typ[Null] = Typ(implicitly)
}

trait ArithStr extends Arith with BaseStr {
  //todo removed below
  //implicit def unit(x: Double) = x.toString

  implicit def intTyp: Typ[Int] = Typ(implicitly)
  implicit def doubleTyp: Typ[Double] = Typ(implicitly)

  def infix_+(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) = "(%s+%s)".format(x,y)
  def infix_-(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) = "(%s-%s)".format(x,y)
  def infix_*(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) = "(%s*%s)".format(x,y)
  def infix_/(x: Rep[Double], y: Rep[Double])(implicit pos: SourceContext) = "(%s/%s)".format(x,y)
}

object Main extends IO {
  val under = "Power"
  
  def main(args: Array[String]): Unit = {
    def first()
    {
      val o = new Power1 with ArithStr
      import o._

      val r = power(infix_+("x0","x1"),4)
      println(r)
    }
    def second()
    {
      val o = new Power2 with ArithStr
      import o._

      val r = power(infix_+("x0","x1"),4)
      println(r)
    }
    def third()
    {
      val o = new Power1 with ArithExp
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExportGraph { val IR: o.type = o }
      p.emitDepGraph(r, prefix+"power1-dot")
    }
    def fourth()
    {
      val o = new Power1 with ArithExpOpt
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExportGraph { val IR: o.type = o }
      p.emitDepGraph(r, prefix+"power2-dot")
    }
    def fifth()
    {
      val o = new Power1 with ArithExpOpt
      import o._
      val f = (x: Rep[Double]) => power(x + x, 4)
      val p = new ScalaGenFlat with ScalaGenArith { val IR: o.type = o }
      p.emitSource(f, "Power2", new PrintWriter(System.out))
    }
    def sixth()
    {
      val o = new Power2 with ArithExpOpt
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      val p = new ExportGraph { val IR: o.type = o }
      p.emitDepGraph(r, prefix+"power3-dot")
    }
    def seventh()
    {
      val o = new Power2 with ArithExpOpt
      import o._
      val f = (x: Rep[Double]) => power(x + x, 4)
      val p = new ScalaGenFlat with ScalaGenArith { val IR: o.type = o }
      p.emitSource(f, "Power3", new PrintWriter(System.out))
    }
    def eighth()
    {
      val o = new Power1 with ArithExpOpt with CompileScala { self => 
        val codegen = new ScalaGenFlat with ScalaGenArith { val IR: self.type = self }
      }
      import o._

      val power4 = (x:Rep[Double]) => power(x,4)
      codegen.emitSource(power4, "Power4", new PrintWriter(System.out))
      val power4c = compile(power4)
      println(power4c(2))
    }
    println("first")
    first()
    println("second")
    second()
    println("third")
    third()
    println("fourth")
    fourth()
    println("fifth")
    fifth()
    println("sixth")
    sixth()
    println("seventh")
    seventh()
    println("eighth")
    eighth()
    // trait Prog extends Base with NumericOps with PrimitiveOps with StructOps with LiftNumeric {
    //   def test(x: Rep[Int]) = {
    //     println("Hello world")
    //     val a = unit(2.0)
    //     val f1 = (1.0 + a) // this one is ok

    //     val struct = new Record {  // FIXME: each of the statements below cause compiler errors ("erroneous or inaccessible type")
    //         //val f1 = (1.0 + a):Rep[Double]
    //         //val f1 = (1.0 + a)
    //         //val f1 = { val z = (1.0 + a); z }
    //         val dummy = 1.0
    //     }
    //   }
    // }
  }
}

