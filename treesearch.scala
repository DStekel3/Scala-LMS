import scala.lms.common._
import scala.collection.immutable.Queue

object TreeSearch extends IO {
  val under = "TreeSearch" 

  def run() = {
    // List of integers
    val nums: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val tree = MyTree.fromSortedList(nums)
    println("contains(5): "+tree.contains(5))
    firstSnippet()
  }

  def firstSnippet() = {
    println("Generating first code snippet in /out/"+under+"-1.actual.scala")
    val snippet = new DslDriver[Int,Boolean] {
      def snippet(x: Rep[Int]) = {
        val nums: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        val tree = MyTree.fromSortedList(nums)

        // val array: Array[Int] = Array(5, 4, 3, 2, 1, 0)
        // val i: Rep[Int] = 2
        // val result: Rep[Int] = array(i) // Error: can't index into Array[Int] with a Rep[Int], Int required
        // val runtimeArray = staticData(array)
        // val result: Rep[Int] = runtimeArray(i) // Works:

        tree.contains(5)
      }
    }
    assert(snippet.eval(5) == true)
    exec("-TreeSearch", snippet.code)
  }
}

trait StagedTree extends Dsl {
  
}

/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Binary Search Tree http://en.wikipedia.org/wiki/Binary_search_tree
 *
 * Insert - O(log n)
 * Lookup - O(log n)  
 * Remove - O(log n)
 *
 * -Notes-
 *
 * This is an efficient implementation of binary search tree. This tree guarantees
 * O(log n) running time for ordered operations like 'nthMin', 'nthMax' and 'rank'.
 * The main idea here - is use additional node field that stores size of tree rotted
 * at this node. This allows to get the size of tree in O(1) instead of linear time.
 */

trait StagesTree extends Dsl{
  abstract sealed class MyTree{
  /**
   * The value of this tree.
   */
  def value: Rep[Int]

  /**
   * The left child of this tree.
   */
  def left: MyTree

  /**
   * The right child of this tree.
   */
  def right: MyTree

  /**
   * The size of this tree.
   */
  def size: Int

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Checks whether this tree contains element 'x' or not.
   *
   * Exercise 2.1 @ PFDS.
   *
   * According to the Anderson's paper (1991) we can reduce the number of comparisons
   * from 2d to d + 1 in the worst case by keeping track of candidate elements that migh
   * be equal to the query.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def contains(x: Rep[Int]): Rep[Boolean] = {
    def loop(t: MyTree, c: Option[Rep[Int]]): Rep[Boolean] = 
      if (t.isEmpty) check(c)
      else if (x < t.value) loop(t.left, c)
      else loop(t.right, Some(t.value))

    def check(c: Option[Rep[Int]]): Rep[Boolean] = c match {
      case Some(y) if y == x => true
      case _ => false
    }

    loop(this, None)
  }

  /**
   * Fails with given message 'm'.
   */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case object MyLeaf extends MyTree {
  def value: Nothing = fail("An empty tree.")
  def left: MyTree = fail("An empty tree.")
  def right: MyTree = fail("An empty tree.")
  def size: Int = 0

  def isEmpty: Boolean = true
}

case class MyBranch(value: Rep[Int], 
                    left: MyTree, 
                    right: MyTree, 
                    size: Int) extends MyTree {
  def isEmpty: Boolean = false
}

object MyTree {

  /**
   * An empty tree.
   */
  def empty[Int]: MyTree = MyLeaf

  /**
   * A smart constructor for tree's branch.
   * 
   * Time - O(1)
   * Space - O(1)
   */
  def make(x: Int, l: MyTree =MyLeaf, r: MyTree = MyLeaf): MyTree =
    MyBranch(x, l, r, l.size + r.size + 1)

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
  def fromSortedList(l: List[Int]): MyTree = {
    def loop(ll: List[Int], n: Int): (List[Int], MyTree) = 
      if (n == 0) (ll, MyTree.empty)
      else {
        val (lt, left) = loop(ll, n/2)
        val (rt, right) = loop(lt.tail, n-1-n/2)
        (rt, MyTree.make(lt.head, left, right))
      }

    loop(l, l.length)._2
  }
}
}

abstract sealed class MyTree{
  /**
   * The value of this tree.
   */
  def value: Int

  /**
   * The left child of this tree.
   */
  def left: MyTree

  /**
   * The right child of this tree.
   */
  def right: MyTree

  /**
   * The size of this tree.
   */
  def size: Int

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Checks whether this tree contains element 'x' or not.
   *
   * Exercise 2.1 @ PFDS.
   *
   * According to the Anderson's paper (1991) we can reduce the number of comparisons
   * from 2d to d + 1 in the worst case by keeping track of candidate elements that migh
   * be equal to the query.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def contains(x: Int): Boolean = {
    def loop(t: MyTree, c: Option[Int]): Boolean = 
      if (t.isEmpty) check(c)
      else if (x < t.value) loop(t.left, c)
      else loop(t.right, Some(t.value))

    def check(c: Option[Int]): Boolean = c match {
      case Some(y) if y == x => true
      case _ => false
    }

    loop(this, None)
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

case object MyLeaf extends MyTree {
  def value: Nothing = fail("An empty tree.")
  def left: MyTree = fail("An empty tree.")
  def right: MyTree = fail("An empty tree.")
  def size: Int = 0

  def isEmpty: Boolean = true
}

case class MyBranch(value: Int, 
                    left: MyTree, 
                    right: MyTree, 
                    size: Int) extends MyTree {
  def isEmpty: Boolean = false
}

object MyTree {

  /**
   * An empty tree.
   */
  def empty[Int]: MyTree = MyLeaf

  /**
   * A smart constructor for tree's branch.
   * 
   * Time - O(1)
   * Space - O(1)
   */
  def make(x: Int, l: MyTree =MyLeaf, r: MyTree = MyLeaf): MyTree =
    MyBranch(x, l, r, l.size + r.size + 1)

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
  def fromSortedList(l: List[Int]): MyTree = {
    def loop(ll: List[Int], n: Int): (List[Int], MyTree) = 
      if (n == 0) (ll, MyTree.empty)
      else {
        val (lt, left) = loop(ll, n/2)
        val (rt, right) = loop(lt.tail, n-1-n/2)
        (rt, MyTree.make(lt.head, left, right))
      }

    loop(l, l.length)._2
  }
}