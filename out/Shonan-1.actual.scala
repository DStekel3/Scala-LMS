/*****************************************
Emitting Generated Code
*******************************************/
class Snippet(px6:Array[Int]) extends ((Array[Int])=>(Array[Int])) {
  def apply(x38:Array[Int]): Array[Int] = {
    val x39 = new Array[Int](5)
    val x6 = px6 // static data: Array(1,1,1,1,1)
    var x40 : Int = 0
    val x48 = while (x40 < 5) {
      val x41 = x39(0)
      val x42 = x6(x40)
      val x43 = x38(x40)
      val x44 = x42 * x43
      val x45 = x41 + x44
      val x46 = x39(0) = x45
      x40 = x40 + 1
    }
    val x49 = x39(1)
    val x51 = x39(1) = x49
    val x56 = x39(2)
    val x57 = x39(2) = x56
    val x53 = x38(2)
    val x58 = x56 + x53
    val x59 = x39(2) = x58
    val x60 = x39(3)
    val x61 = x39(3) = x60
    val x62 = x39(4)
    val x63 = x39(4) = x62
    val x64 = x62 + x53
    val x65 = x39(4) = x64
    val x55 = x38(4)
    val x66 = x64 + x55
    val x67 = x39(4) = x66
    x39
  }
}
/*****************************************
End of Generated Code
*******************************************/
