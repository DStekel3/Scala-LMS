/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((java.lang.String)=>(Boolean)) {
  def apply(x62:java.lang.String): Boolean = {
    var x63: Int = -1
    var x64: Boolean = false
    val x68 = x62.length
    val x121 = while ({val x65 = x64
      val x71 = if (x65) {
        false
      } else {
        val x67 = x63
        val x69 = x67 < x68
        x69
      }
      x71}) {
      val x73 = x63 += 1
      val x74 = x63
      val x75 = x74 < x68
      val x78 = if (x75) {
        val x76 = x62(x74)
        val x77 = 'h' == x76
        x77
      } else {
        false
      }
      val x118 = if (x78) {
        val x79 = x74 + 1
        val x80 = x79 < x68
        val x83 = if (x80) {
          val x81 = x62(x79)
          val x82 = 'e' == x81
          x82
        } else {
          false
        }
        val x117 = if (x83) {
          val x84 = x79 + 1
          var x85: Int = x84
          val x86 = x84 == x68
          var x87: Boolean = x86
          var x88: Boolean = false
          val x110 = while ({val x89 = x88
            val x94 = if (x89) {
              false
            } else {
              val x91 = x87
              val x92 = !x91
              x92
            }
            val x98 = if (x94) {
              val x95 = x85
              val x96 = x95 < x68
              x96
            } else {
              false
            }
            x98}) {
            val x100 = x85
            val x101 = x62(x100)
            val x102 = 'l' == x101
            val x103 = !x102
            x88 = x103
            val x105 = x85 += 1
            val x106 = x85
            val x107 = x106 == x68
            x87 = x107
            ()
          }
          val x111 = x88
          val x115 = if (x111) {
            false
          } else {
            val x113 = x87
            x113
          }
          x115
        } else {
          false
        }
        x117
      } else {
        false
      }
      x64 = x118
      ()
    }
    val x122 = x64
    x122
  }
}
/*****************************************
End of Generated Code
*******************************************/
