import scala.lms.common._

object TreeSearch extends IO {
  val under = "TreeSearch" 

  def run() = {
    // List of integers
    val nums: List[Int] = List(1, 2, 3, 4)
    val result = BinarySearch(nums, 5)   
    println(result)
  }

  def BinarySearch[A <% Ordered[A]](list: List[A], key: A): Option[A] = {
    def search(l: List[A], r: List[A]): Option[A] =
      if (l == r) None
      else test(l, r, middle(l, r))

    def test(l: List[A], r: List[A], m: List[A]): Option[A] =
      if (key < m.head) search(l, m)
      else if (key > m.head) search(m.tail, r)
      else Some(m.head)

    def middle(l: List[A], r: List[A]): List[A] = {
      def race(t: List[A], h: List[A]): List[A] =
        if (h != r && h.tail != r)
          race(t.tail, h.tail.tail)
        else t

      race(l, l.tail)
    }

    search(list, Nil)
  }
}