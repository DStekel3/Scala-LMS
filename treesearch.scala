import scala.lms.common._
import scala.collection.mutable.ListBuffer

/**
 * This code is based on Scalacaster project, https://github.com/vkostyukov/scalacaster
 * which is written by Vladimir Kostyukov, http://vkostyukov.ru
 * Since the original implementation works with a generic type, which LMS does not know to to stage, we needed to make adjustments.
 * The adjusted Scala-implementation is shown at the bottom of this file. This implementation is being used as the unstaged version.
 * The implementation which works with staged code. This implementation generated code, which is being used as the staged version.
 * You can find the generated code in the "/out" folder.
 * Binary Search Tree http://en.wikipedia.org/wiki/Binary_search_tree
**/

object TreeSearch extends IO {
  val under = "TreeSearch"

  // size of the balanced binary tree
  val sizeOfTree = 250

  // list of values which is the content of the tree
  val nums: List[Int] = List.range(1, sizeOfTree+1)

  // random object, used to generate a random number
  val random = scala.util.Random
  
  // list where we keep track of the results of the 'default' algorithm
  var unstagedResults  = new ListBuffer[Long]()

  // list where we keep track of the results of the staged algorithm
  var stagedResults = new ListBuffer[Long]()

  // # of times we search for a value in the tree
  val numberOfTimes = 1000000

  // Snippet which, when called, calls the staged version of the Tree.contains()-method
  val snippet = new DslDriver[Int,Boolean] with StagedVersion {
      val tree = Tree.fromSortedList(nums)
      def snippet(x: Rep[Int]) = {
        tree.contains(x)
      }
    }

  def run() = {
    // generate staged code. See this code in the /out folder
    snippet.precompile
    println("Generated staged code. Order of output = (unstaged-staged) in milliseconds.")
    exec("-TreeSearch"+sizeOfTree, snippet.code)
    
    // initialization code
    val a = 0
    for(a <- 1 to 1000)
    {
      snippet.eval(a)
    }
    
    // run experiment once, used for initialization
    System.gc
    
    val tree = Tree.fromSortedList(nums)
    for(z <- 1 to numberOfTimes){
      val num = random.nextInt(sizeOfTree)
      tree.contains(num)
    }
    System.gc
    for(z <- 1 to numberOfTimes){
      val num = random.nextInt(sizeOfTree)
      snippet.eval(num)
    }
    println("Initialization finished.")

    // run experiment 30 times and save results in a list
    val z = 1
    for(z <- 1 to 30)
    {
      System.gc
      unstagedRun()
      System.gc
      stagedRun()
      println("")
    }

    // print average of experiments
    println("Average millis unstaged: "+mean(unstagedResults.toList))
    println("Average millis staged:   "+mean(stagedResults.toList))
  }

  // method which computes the mean of a list
  def mean[T](item:Traversable[T])(implicit n:Numeric[T]) = {
  n.toDouble(item.sum) / item.size.toDouble
  }

  // method where we create a tree and search (in the default way) for a random value (numberOfTimes) times.
  def unstagedRun() = {
    val tree = Tree.fromSortedList(nums)
    val t1 = System.nanoTime();
    val z = 1
    for(z <- 1 to numberOfTimes){
      val num = random.nextInt(sizeOfTree)
      tree.contains(num)
    }
    val t2 = System.nanoTime();
    val millis = (t2 - t1)/1000000
    println(millis)
    unstagedResults += millis
  }

  // method where we create a tree and search (in the staged way) for a random value (numberOfTimes) times.
  def stagedRun() = {
    val t1 = System.nanoTime();
    val z = 1
    for(z <- 1 to numberOfTimes){
      val num = random.nextInt(sizeOfTree)
      snippet.eval(num)
    }
    val t2 = System.nanoTime();
    val millis = (t2 - t1)/1000000
    println(millis)
    stagedResults += millis
  }
}

// Staged implementation of the Binary Tree
trait StagedVersion extends Dsl{
  abstract sealed class Tree {
  /**
   * The value of this tree.
   */
  def value: Rep[Int]

  /**
   * The left child of this tree.
   */
  def left: Tree

  /**
   * The right child of this tree.
   */
  def right: Tree

  /**
   * The size of this tree.
   */
  def size: Int

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

   def contains(x: Rep[Int]): Rep[Boolean] = {
    def loop(t: Tree): Rep[Boolean] = 
      if (t.isEmpty) false
      else if (t.value == x) true
      else if (x < t.value) loop(t.left)
      else loop(t.right)
    
    loop(this)
  }

  /**
   * Fails with given message 'm'.
   */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case object Leaf extends Tree {
  def value: Nothing = fail("An empty tree.")
  def left: Tree = fail("An empty tree.")
  def right: Tree = fail("An empty tree.")
  def size: Int = 0

  def isEmpty: Boolean = true
}

case class Branch(value: Rep[Int], 
                    left: Tree, 
                    right: Tree, 
                    size: Int) extends Tree {
  def isEmpty: Boolean = false
}

object Tree {

  /**
   * An empty tree.
   */
  def empty[Int]: Tree = Leaf

  /**
   * A smart constructor for tree's branch.
   * 
   * Time - O(1)
   * Space - O(1)
   */
  def make(x: Int, l: Tree =Leaf, r: Tree = Leaf): Tree =
    Branch(x, l, r, l.size + r.size + 1)

  /**
   * Creates a new balanced tree from given sorted list 'l'.
   *
   * http://www.geeksforgeeks.org/sorted-linked-list-to-balanced-bst/
   * 
   * TODO There should be a way to do it better.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fromSortedList(l: List[Int]): Tree = {
    def loop(ll: List[Int], n: Int): (List[Int], Tree) = 
      if (n == 0) (ll, Tree.empty)
      else {
        val (lt, left) = loop(ll, n/2)
        val (rt, right) = loop(lt.tail, n-1-n/2)
        (rt, Tree.make(lt.head, left, right))
      }

    loop(l, l.length)._2
  }
}
}


/*
* Implementation of the Binary Tree, which creates a balanced tree from a list of integers.
*/
abstract sealed class Tree{
  /**
   * The value of this tree.
   */
  def value: Int

  /**
   * The left child of this tree.
   */
  def left: Tree

  /**
   * The right child of this tree.
   */
  def right: Tree

  /**
   * The size of this tree.
   */
  def size: Int

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

  def contains(x: Int): Boolean = {
    def loop(t: Tree): Boolean = 
      if (t.isEmpty) false
      else if (t.value == x) true
      else if (x < t.value) loop(t.left)
      else loop(t.right)
    
    loop(this)
  }

  /**
   * Converts this tree into the string representation.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  override def toString: String = 
    if (isEmpty) "."
    else "{" + left + value + right + "}"


  /**
   * Fails with given message 'm'.
   */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case object Leaf extends Tree {
  def value: Nothing = fail("An empty tree.")
  def left: Tree = fail("An empty tree.")
  def right: Tree = fail("An empty tree.")
  def size: Int = 0

  def isEmpty: Boolean = true
}

case class Branch(value: Int, 
                    left: Tree, 
                    right: Tree, 
                    size: Int) extends Tree {
  def isEmpty: Boolean = false
}

object Tree {

  /**
   * An empty tree.
   */
  def empty[Int]: Tree = Leaf

  /**
   * A smart constructor for tree's branch.
   * 
   * Time - O(1)
   * Space - O(1)
   */
  def make(x: Int, l: Tree =Leaf, r: Tree = Leaf): Tree =
    Branch(x, l, r, l.size + r.size + 1)

  /**
   * Creates a new balanced tree from given sorted list 'l'.
   *
   * http://www.geeksforgeeks.org/sorted-linked-list-to-balanced-bst/
   * 
   * TODO There should be a way to do it better.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fromSortedList(l: List[Int]): Tree = {
    def loop(ll: List[Int], n: Int): (List[Int], Tree) = 
      if (n == 0) (ll, Tree.empty)
      else {
        val (lt, left) = loop(ll, n/2)
        val (rt, right) = loop(lt.tail, n-1-n/2)
        (rt, Tree.make(lt.head, left, right))
      }

    loop(l, l.length)._2
  }
}
