import scala.lms.common._

/**
  * The Rabin-Karp algorithm needs to be able to calculate the modulus of two Ints.
  * LMS provides a % operator that works on staged Ints.
  * Unfortunately, the % operator is defined in a counterintuitive way
  * in the Scala language for negative numbers:
  *     3 % 8 = 3
  *    -3 % 8 = -3
  *
  * We have added a new operator, %%, that relies on Scala's Math.floorMod() function.
  * This works correctly:
  *     3 %% 8 = 3
  *    -3 %% 8 = 5
  *
  * Sadly, LMS does not provide a pre-defined floodMod() function. So we have to add a new
  * %% operator that works on Rep[Int]s; and compiles down to a call to floorMod().
  *
  * The IntOps trait defines the interface of the new staged operation on Rep[Int]s
  * (the %% operator). It works for any combination of Ints and Rep[Int]s.
  * (For a version of %% that simply works on two unstaged Ints, see RabinKarp.scala)
  */
trait IntOps extends Base with PrimitiveOps {
  def infix_%%(x: Int     , y: Rep[Int]): Rep[Int] = floor_mod(unit(x), y)
  def infix_%%(x: Rep[Int], y: Int     ): Rep[Int] = floor_mod(x, unit(y))
  def infix_%%(x: Rep[Int], y: Rep[Int]): Rep[Int] = floor_mod(x, y)

  def floor_mod(x: Rep[Int], y: Rep[Int]): Rep[Int]
}

/**
  * The IntExp trait implements the previously defined floor_mod() function,
  * which calculates the floor mod. We return a new IR node, FloorMod.
  */
trait IntExp extends IntOps with BaseExp with PrimitiveOpsExp {
  case class FloorMod(x: Exp[Int], y: Exp[Int]) extends Def[Int]
  
  def floor_mod(x: Exp[Int], y: Exp[Int]) = FloorMod(x, y)
}

/**
  * The ScalaGenIntOps trait extends the Scala code generator provided by LMS.
  * When a FloorMod node is encountered for certain integers x and y, we emit the string
  * "Math.floodMod(x, y)" in the generated code.
  */
trait ScalaGenIntOps extends ScalaGenBase {
  val IR: IntExp
  import IR._
  
  override def emitNode(sym: Sym[Any], node: Def[Any]) = node match {
    case FloorMod(x, y) => emitValDef(sym, "Math.floorMod(" + quote(x) + ", " + quote(y) + ")")
    case _ => super.emitNode(sym, node)
  }
}

/**
  * Once all these traits have been defined, they have to be mixed in in the appropriate places.
  * The dslapi.scala file (which was borrowed from the Scala tutorials) defines a DslDriver class
  * with a staged function that can be implemented; we use this for our experiments.
  * The newly defined traits have to be mixed in that DslDriver class; see dslapi.scala.
  * There are three places in that file, marked with the comment  --- Add new operations here --- 
  * where we add these new traits.
  */