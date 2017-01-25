import scala.lms.common._

trait CharOps extends Base with PrimitiveOps {
  implicit def repCharToRepInt(x: Rep[Char]): Rep[Int] = x.toInt

  implicit def charToCharOps(c: Char): CharOpsCls = new CharOpsCls(unit(c))
  implicit def repCharToCharOps(c: Rep[Char]): CharOpsCls = new CharOpsCls(c)
  implicit def varCharToCharOps(c: Var[Char]): CharOpsCls = new CharOpsCls(readVar(c))

  class CharOpsCls(c: Rep[Char]) {
    def toInt: Rep[Int] = char_to_int(c)
  }
  def char_to_int(c: Rep[Char]): Rep[Int]
}

trait CharExp extends CharOps with BaseExp with PrimitiveOpsExp {
  case class CharToInt(c: Exp[Char]) extends Def[Int]

  def char_to_int(c: Exp[Char]) = CharToInt(c)
}

trait ScalaGenCharOps extends ScalaGenBase {
  val IR: CharExp
  import IR._
  
  override def emitNode(sym: Sym[Any], node: Def[Any]) = node match {
    case CharToInt(c) => emitValDef(sym, quote(c) + ".toInt")
    case _ => super.emitNode(sym, node)
  }
}