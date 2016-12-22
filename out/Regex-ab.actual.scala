/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((java.lang.String)=>(Boolean)) {
  def apply(x30:java.lang.String): Boolean = {
    var x31: Int = -1
    var x32: Boolean = false
    val x36 = x30.length
    val x57 = while ({val x33 = x32
      val x39 = if (x33) {
        false
      } else {
        val x35 = x31
        val x37 = x35 < x36
        x37
      }
      x39}) {
      val x41 = x31 += 1
      val x42 = x31
      val x43 = x42 < x36
      val x46 = if (x43) {
        val x44 = x30(x42)
        val x45 = 'a' == x44
        x45
      } else {
        false
      }
      val x54 = if (x46) {
        val x47 = x42 + 1
        val x48 = x47 < x36
        val x51 = if (x48) {
          val x49 = x30(x47)
          val x50 = 'b' == x49
          x50
        } else {
          false
        }
        val x53 = x51
        x53
      } else {
        false
      }
      x32 = x54
      ()
    }
    val x58 = x32
    x58
  }
}
/*****************************************
End of Generated Code
*******************************************/
