/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((java.lang.String)=>(Boolean)) {
  def apply(x60:java.lang.String): Boolean = {
    var x61: Int = -1
    var x62: Boolean = false
    val x66 = x60.length
    val x117 = while ({val x63 = x62
      val x69 = if (x63) {
        false
      } else {
        val x65 = x61
        val x67 = x65 < x66
        x67
      }
      x69}) {
      val x71 = x61 += 1
      val x72 = x61
      val x73 = x72 < x66
      val x76 = if (x73) {
        val x74 = x60(x72)
        val x75 = 'h' == x74
        x75
      } else {
        false
      }
      val x114 = if (x76) {
        val x77 = x72 + 1
        val x78 = x77 < x66
        val x81 = if (x78) {
          val x79 = x60(x77)
          val x80 = 'e' == x79
          x80
        } else {
          false
        }
        val x113 = if (x81) {
          val x82 = x77 + 1
          var x83: Int = x82
          var x84: Boolean = true
          var x85: Boolean = false
          val x106 = while ({val x86 = x85
            val x91 = if (x86) {
              false
            } else {
              val x88 = x84
              val x89 = !x88
              x89
            }
            val x95 = if (x91) {
              val x92 = x83
              val x93 = x92 < x66
              x93
            } else {
              false
            }
            x95}) {
            val x97 = x83
            val x98 = x60(x97)
            val x99 = 'l' == x98
            val x100 = !x99
            x85 = x100
            val x102 = x83 += 1
            val x103 = x83
            x84 = true
            ()
          }
          val x107 = x85
          val x111 = if (x107) {
            false
          } else {
            val x109 = x84
            x109
          }
          x111
        } else {
          false
        }
        x113
      } else {
        false
      }
      x62 = x114
      ()
    }
    val x118 = x62
    x118
  }
}
/*****************************************
End of Generated Code
*******************************************/
