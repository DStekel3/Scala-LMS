/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((java.lang.String)=>(Boolean)) {
  def apply(x42:java.lang.String): Boolean = {
    var x43: Int = -1
    var x44: Boolean = false
    val x48 = x42.length
    val x81 = while ({val x45 = x44
      val x51 = if (x45) {
        false
      } else {
        val x47 = x43
        val x49 = x47 < x48
        x49
      }
      x51}) {
      val x53 = x43 += 1
      val x54 = x43
      val x55 = x54 < x48
      val x58 = if (x55) {
        val x56 = x42(x54)
        val x57 = 'h' == x56
        x57
      } else {
        false
      }
      val x78 = if (x58) {
        val x59 = x54 + 1
        val x60 = x59 < x48
        val x63 = if (x60) {
          val x61 = x42(x59)
          val x62 = 'e' == x61
          x62
        } else {
          false
        }
        val x77 = if (x63) {
          val x64 = x59 + 1
          val x65 = x64 < x48
          val x68 = if (x65) {
            val x66 = x42(x64)
            val x67 = 'l' == x66
            x67
          } else {
            false
          }
          val x76 = if (x68) {
            val x69 = x64 + 1
            val x70 = x69 < x48
            val x73 = if (x70) {
              val x71 = x42(x69)
              val x72 = 'l' == x71
              x72
            } else {
              false
            }
            val x75 = x73
            x75
          } else {
            false
          }
          x76
        } else {
          false
        }
        x77
      } else {
        false
      }
      x44 = x78
      ()
    }
    val x82 = x44
    x82
  }
}
/*****************************************
End of Generated Code
*******************************************/
