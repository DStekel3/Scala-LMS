/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((java.lang.String)=>(Boolean)) {
  def apply(x59:java.lang.String): Boolean = {
    var x60: Int = -1
    var x61: Boolean = false
    val x65 = x59.length
    val x115 = while ({val x62 = x61
      val x68 = if (x62) {
        false
      } else {
        val x64 = x60
        val x66 = x64 < x65
        x66
      }
      x68}) {
      val x70 = x60 += 1
      val x71 = x60
      var x72: Int = x71
      val x73 = x71 < x65
      val x76 = if (x73) {
        val x74 = x59(x71)
        val x75 = 'b' == x74
        x75
      } else {
        false
      }
      val x78 = x76
      var x79: Boolean = x78
      var x80: Boolean = false
      val x107 = while ({val x81 = x80
        val x86 = if (x81) {
          false
        } else {
          val x83 = x79
          val x84 = !x83
          x84
        }
        val x90 = if (x86) {
          val x87 = x72
          val x88 = x87 < x65
          x88
        } else {
          false
        }
        x90}) {
        val x92 = x72
        val x93 = x59(x92)
        val x94 = 'a' == x93
        val x95 = !x94
        x80 = x95
        val x97 = x72 += 1
        val x98 = x72
        val x99 = x98 < x65
        val x102 = if (x99) {
          val x100 = x59(x98)
          val x101 = 'b' == x100
          x101
        } else {
          false
        }
        val x104 = x102
        x79 = x104
        ()
      }
      val x108 = x80
      val x112 = if (x108) {
        false
      } else {
        val x110 = x79
        x110
      }
      x61 = x112
      ()
    }
    val x116 = x61
    x116
  }
}
/*****************************************
End of Generated Code
*******************************************/
