/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((java.lang.String)=>(Boolean)) {
  def apply(x39:java.lang.String): Boolean = {
    val x40 = x39.length
    val x41 = 0 < x40
    val x44 = if (x41) {
      val x42 = x39(0)
      val x43 = 'a' == x42
      x43
    } else {
      false
    }
    val x77 = if (x44) {
      var x45: Int = 1
      val x46 = 1 == x40
      var x47: Boolean = x46
      var x48: Boolean = false
      val x70 = while ({val x49 = x48
        val x54 = if (x49) {
          false
        } else {
          val x51 = x47
          val x52 = !x51
          x52
        }
        val x58 = if (x54) {
          val x55 = x45
          val x56 = x55 < x40
          x56
        } else {
          false
        }
        x58}) {
        val x60 = x45
        val x61 = x39(x60)
        val x62 = 'b' == x61
        val x63 = !x62
        x48 = x63
        val x65 = x45 += 1
        val x66 = x45
        val x67 = x66 == x40
        x47 = x67
        ()
      }
      val x71 = x48
      val x75 = if (x71) {
        false
      } else {
        val x73 = x47
        x73
      }
      x75
    } else {
      false
    }
    x77
  }
}
/*****************************************
End of Generated Code
*******************************************/
