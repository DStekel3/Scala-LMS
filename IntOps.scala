import scala.lms.common._

trait IntOps extends Base with PrimitiveOps {
  def infix_%%(x: Int     , y: Rep[Int]): Rep[Int] = floor_mod(unit(x), y)
  def infix_%%(x: Rep[Int], y: Int     ): Rep[Int] = floor_mod(x, unit(y))
  def infix_%%(x: Rep[Int], y: Rep[Int]): Rep[Int] = floor_mod(x, y)

  def floor_mod(x: Rep[Int], y: Rep[Int]): Rep[Int]
}

trait IntExp extends IntOps with BaseExp with PrimitiveOpsExp {
  case class FloorMod(x: Exp[Int], y: Exp[Int]) extends Def[Int]
  
  def floor_mod(x: Exp[Int], y: Exp[Int]) = FloorMod(x, y)
}

trait ScalaGenIntOps extends ScalaGenBase {
  val IR: IntExp
  import IR._
  
  override def emitNode(sym: Sym[Any], node: Def[Any]) = node match {
    case FloorMod(x, y) => emitValDef(sym, "Math.floorMod(" + quote(x) + ", " + quote(y) + ")")
    case _ => super.emitNode(sym, node)
  }
}