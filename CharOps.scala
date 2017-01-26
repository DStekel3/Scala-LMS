import scala.lms.common._

/**
  * The Rabin-Karp algorithm needs to be able to do arithmetic
  * on a mix of Chars and Ints. In regular Scala, Chars
  * will be implicitly converted to Ints where needed:
  *     100 + 'a'
  *     >res0: Int = 197
  *
  * This doesn't happen when you're working with Rep[Char]s and Rep[Int]s.
  * We have added a new .toInt method on Rep[Char]s, and added an implicit
  * conversion from Rep[Char]s to Rep[Int]s that makes use of this new .toInt method.
  *
  * This way, when a Rep[Char] and Rep[Int] have to be added, for example,
  * the Rep[Char] will be converted to a Rep[Int] via .toInt, and then the two
  * Rep[Int]s will be added together, yielding another Rep[Int].
  *
  * The CharOps trait defines the interface of the new staged operation on Rep[Char]s
  * (the .toInt method) and also defines the implicit conversion from staged Chars to Ints.
  */
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

/**
  * The CharExp trait implements the previously defined char_to_int() function,
  * which takes a char and returns an int. We return a new IR node, CharToInt.
  */
trait CharExp extends CharOps with BaseExp with PrimitiveOpsExp {
  case class CharToInt(c: Exp[Char]) extends Def[Int]

  def char_to_int(c: Exp[Char]) = CharToInt(c)
}

/**
  * The ScalaGenCharOps trait extends the Scala code generator provided by LMS.
  * When a CharToInt node is encountered for a certain character c, we emit the string
  * "c.toInt" in the generated code.
  */
trait ScalaGenCharOps extends ScalaGenBase {
  val IR: CharExp
  import IR._
  
  override def emitNode(sym: Sym[Any], node: Def[Any]) = node match {
    case CharToInt(c) => emitValDef(sym, quote(c) + ".toInt")
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