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

abstract sealed class MyTree {
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
   * Adds given element 'x' into this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def add(x: Int): MyTree =
    if (isEmpty) MyTree.make(x)
    else if (x < value) MyTree.make(value, left.add(x), right)
    else if (x > value) MyTree.make(value, left, right.add(x))
    else this

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
   * Searches for the minimal element of this tree.
   * 
   * Time - O(log n)
   * Space - O(log n)
   */
  def min: Int = {
    def loop(t: MyTree, m: Int): Int = 
      if (t.isEmpty) m
      else loop(t.left, t.value)

    if (isEmpty) fail("An empty tree.")
    else loop(left, value)
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

abstract sealed class Tree[+A <% Ordered[A]] {

  /**
   * The value of this tree.
   */
  def value: A

  /**
   * The left child of this tree.
   */
  def left: Tree[A]

  /**
   * The right child of this tree.
   */
  def right: Tree[A]

  /**
   * The size of this tree.
   */
  def size: Int

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

 
  /**
   * Adds given element 'x' into this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def add[B >: A <% Ordered[B]](x: B): Tree[B] =
    if (isEmpty) Tree.make(x)
    else if (x < value) Tree.make(value, left.add(x), right)
    else if (x > value) Tree.make(value, left, right.add(x))
    else this

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
  def contains[B >: A <% Ordered[B]](x: B): Boolean = {
    def loop(t: Tree[A], c: Option[A]): Boolean = 
      if (t.isEmpty) check(c)
      else if (x < t.value) loop(t.left, c)
      else loop(t.right, Some(t.value))

    def check(c: Option[A]): Boolean = c match {
      case Some(y) if y == x => true
      case _ => false
    }

    loop(this, None)
  }

  /**
   * Searches for the minimal element of this tree.
   * 
   * Time - O(log n)
   * Space - O(log n)
   */
  def min: A = {
    def loop(t: Tree[A], m: A): A = 
      if (t.isEmpty) m
      else loop(t.left, t.value)

    if (isEmpty) fail("An empty tree.")
    else loop(left, value)
  }

  /**
   * Searches for the maximal element of this tree.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def max: A = {
    def loop(t: Tree[A], m: A): A = 
      if (t.isEmpty) m
      else loop(t.right, t.value)

    if (isEmpty) fail("An empty tree.")
    else loop(right, value)
  }

  /**
   * Constructs the list of 'n' largest elements of this tree.
   *
   * Note: We suppose that list.size runs in O(1).
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def takeLargest(n: Int): List[A] = {
    def loop(t: Tree[A], l: List[A]): List[A] = 
      if (t.isEmpty || l.size == n) l
      else {
        val ll = loop(t.right, l)
        if (ll.size == n) ll
        else loop(t.left, t.value :: ll)
      }

    loop(this, Nil).reverse
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
   * Converts this tree into linked list.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def toList: List[A] = {
    def loop(t: Tree[A], l: List[A]): List[A] = 
      if (t.isEmpty) l
      else loop(t.left, t.value :: loop(t.right, l))

    loop(this, Nil)
  }

  /**
   * Fails with given message 'm'.
   */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case object Leaf extends Tree[Nothing] {
  def value: Nothing = fail("An empty tree.")
  def left: Tree[Nothing] = fail("An empty tree.")
  def right: Tree[Nothing] = fail("An empty tree.")
  def size: Int = 0

  def isEmpty: Boolean = true
}

case class Branch[A <% Ordered[A]](value: A, 
                                   left: Tree[A], 
                                   right: Tree[A], 
                                   size: Int) extends Tree[A] {
  def isEmpty: Boolean = false
}

object Tree {

  /**
   * An empty tree.
   */
  def empty[A]: Tree[A] = Leaf

  /**
   * A smart constructor for tree's branch.
   * 
   * Time - O(1)
   * Space - O(1)
   */
  def make[A <% Ordered[A]](x: A, l: Tree[A] = Leaf, r: Tree[A] = Leaf): Tree[A] =
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
  def fromSortedList[A <% Ordered[A]](l: List[A]): Tree[A] = {
    def loop(ll: List[A], n: Int): (List[A], Tree[A]) = 
      if (n == 0) (ll, Tree.empty)
      else {
        val (lt, left) = loop(ll, n / 2)
        val (rt, right) = loop(lt.tail, n - 1 - n / 2)
        (rt, Tree.make(lt.head, left, right))
      }

    loop(l, l.length)._2
  }
}