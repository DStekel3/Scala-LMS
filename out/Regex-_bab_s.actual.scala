/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((java.lang.String)=>(Boolean)) {
  def apply(x37:java.lang.String): Boolean = {
    val x38 = x37.length
    val x39 = 0 < x38
    val x42 = if (x39) {
      val x40 = x37(0)
      val x41 = 'a' == x40
      x41
    } else {
      false
    }
    val x73 = if (x42) {
      var x43: Int = 1
      var x44: Boolean = true
      var x45: Boolean = false
      val x66 = while ({val x46 = x45
        val x51 = if (x46) {
          false
        } else {
          val x48 = x44
          val x49 = !x48
          x49
        }
        val x55 = if (x51) {
          val x52 = x43
          val x53 = x52 < x38
          x53
        } else {
          false
        }
        x55}) {
        val x57 = x43
        val x58 = x37(x57)
        val x59 = 'b' == x58
        val x60 = !x59
        x45 = x60
        val x62 = x43 += 1
        val x63 = x43
        x44 = true
        ()
      }
      val x67 = x45
      val x71 = if (x67) {
        false
      } else {
        val x69 = x44
        x69
      }
      x71
    } else {
      false
    }
    x73
  }
}
/*****************************************
End of Generated Code
*******************************************/
