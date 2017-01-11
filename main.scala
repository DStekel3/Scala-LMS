import scala.lms.common._

import java.io.PrintWriter

// Consider a function that turns a string into a palindrome:
//   def createPalindrome(s: String) =
//     s + s.reverse
// Now, we want to stage this function.

// First, we change the type of parameter 's' to Rep[String].
// Now, the + operator and reverse method are not defined for Rep[String]!
// We will put the operations on staged strings into another trait, which
// will be mixed in later.

// We wrap our createPalindrome() function into a trait, and add a
// 'this: PalStringOps' self-type annotation: whenever an instance of
// CreatePalindrome is created, an instance of a concrete subclass of
// PalStringOps that provides operations on staged strings must be
// mixed in, too.
// Also, 's.reverse' is changed into reverse(s) because that will be easier
// to stage.
trait CreatePalindrome { this: PalStringOps => 
  def createPalindrome(s: Rep[String]) =
    s + reverse(s)
}

// PalStringOps is an interface trait that defines some staged operations
// on Rep[String]s. In particular, we define the + operator to concatenate
// strings, and a reverse() function that returns a new string with the
// original string's characters in reverse order. Note that the functions here
// are abstract: they are declared, not defined.
trait PalStringOps extends Base {
  def infix_+(x: Rep[String], y: Rep[String]): Rep[String]
  def reverse(s: Rep[String]): Rep[String]
}

// We need to implement these staged string operations, turning them into
// intermediate representation (IR) nodes. These can be optimized and
// compiled later. The BaseExp class (provided by the Scala LMS framework)
// ensures that Rep[T] = Exp[T], so staged types (Rep) can be converted to 
// IR expressions (Exp).
trait PalStringExp extends PalStringOps with BaseExp {
  // This line is required; it has to do with implicitly converting the
  // Plus and Reverse IR nodes to Rep[String]s, so PalStringOps's type
  // signatures are satisfied.
  implicit def stringTyp: Typ[String] = manifestTyp
  
  // These are IR nodes.
  // (Defs, to be more precise, which represent composite operations).
  case class Plus(x: Exp[String], y: Exp[String]) extends Def[String]
  case class Reverse(s: Exp[String]) extends Def[String]

  def infix_+(x: Exp[String], y: Exp[String]) = Plus(x, y)
  def reverse(s: Exp[String]) = Reverse(s)
}

// Now, at this point, the implementation in PalStringExp could be extended
// with optimizations in a new 'PalStringExpOpt extends PalStringExp' trait.
// The functions could be overridden, and using pattern matching on IR nodes,
// the IR representation could be rewritten in a more efficient form
// (for example, when doing arithmetic, '1.0 * x' could be simplified to 'x').
// We do not use an optimized implementation trait for palindromes here.

// Finally, we need to generate code for the new IR nodes. We create a subclass
// of ScalaGenBase that generates code for the IR additions defined in
// PalStringExp. We override the emitNode() function, adding new cases for
// our IR nodes. We use the emitValDef() function here; this usage was copied
// and adapted from the Scala LMS framework code base.
trait ScalaGenPalString extends ScalaGenBase {
  val IR: PalStringExp
  import IR._
  override def emitNode(sym: Sym[Any], node: Def[Any]) = {
    node match {
      case Plus(x, y) => emitValDef(sym, src"$x + $y")
      case Reverse(s) => emitValDef(sym, src"$s.reverse")
      case _ => super.emitNode(sym, node)
    }
  }
}

// In order to create a code generator instance of ScalaGenBase, some functions
// have to be defined; this ScalaGenFlat trait was copied from a tutorial and
// appears to work fine.
trait ScalaGenFlat extends ScalaGenBase {
   import IR._
   type Block[+T] = Exp[T]
   def getBlockResultFull[T](x: Block[T]): Exp[T] = x
   def reifyBlock[T:Typ](x: =>Exp[T]): Block[T] = x
   def traverseBlock[A](block: Block[A]): Unit = {
     buildScheduleForResult(block) foreach traverseStm
   }
}

object Main {
  def main(args: Array[String]): Unit = {
    
    // The unstaged version of createPalindrome.
    def createPalindromeUnstaged(s: String) =
      s + s.reverse

    // Create a new version of CreatePalindrome (the trait containing
    // the staged version) with the right traits mixed in.
    val o = new CreatePalindrome with PalStringExp with CompileScala { self => 
      val codegen = new ScalaGenFlat with ScalaGenPalString { val IR: self.type = self }
    }
    import o._

    // Compile the createPalindrome() function, loading the generated code
    // immediately into the running program. The resulting function is
    // "unstaged" and can work on present-staged values again (i.e. Strings)
    val createPalindromeStaged = compile(createPalindrome)
    println(s"Un-staged: ${createPalindromeUnstaged("I Love Palindromes")}")
    println(s"Staged: ${createPalindromeStaged("I Love Palindromes")}")

    // We can also inspect the generated code by using 'codegen.emitSource()':
    println("Emitting generated source code...")
    codegen.emitSource(createPalindrome, "Palindrome", new PrintWriter(System.out))
  }
}

