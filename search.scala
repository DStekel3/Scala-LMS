import scala.lms.common._

object Search extends IO {
    val under = "Search"
    var input: Array[Int] = Array()

    val snippet = new DslDriver[Int,Int] {

        def snippet(value: Rep[Int]): Rep[Int] = {
            binarySearchStaged(value)
        }

        def binarySearchStaged(value: Rep[Int]): Rep[Int] = {
            val runtimeArray = staticData(input)
            def go(steps: Int): Rep[((Int,Int)) => Int] = { (lo: Rep[Int], hi: Rep[Int]) =>
                generate_comment("bs_"+steps)
                if (steps > scala.math.ceil(scala.math.log(input.size) / scala.math.log(2)))
                    unit(-1)
                else if (lo > hi)
                    -1
                else {
                    val mid: Rep[Int] = lo + (hi - lo) / 2
                    if(runtimeArray(mid) == value)
                        mid
                    else if (runtimeArray(mid) < value)
                        go(steps + 1)(mid + 1, hi)
                    else
                        go(steps + 1)(lo, mid - 1)
                }
            }
            go(0)(0, runtimeArray.length - 1)
        }
    }

    def secondSnippet(key: Int) = {
        snippet.eval(key)
  }

  def binarySearchUnstaged(value: Int): Int = {
    var steps: Int = 0
    def go(lo: Int, hi: Int): Int = {
      if (steps > scala.math.ceil(scala.math.log(input.size) / scala.math.log(2)))
        -1
      else if (lo > hi)
        -1
      else {
        steps = steps + 1
        val mid: Int = lo + (hi - lo) / 2
        if(input(mid) == value)
          mid
        else if (input(mid) < value)
          go(mid + 1, hi)
        else
          go(lo, mid - 1)
      }
    }
    go(0, input.size - 1)
  }

  def run() = {
      output_run(40, 25000)
      output_run(400, 2500)
      output_run(4000, 250)
      output_run(40000, 250)
      
  }

  def output_run(x: Int, y: Int)
  {
    println("======== Run "+x+" : "+y+"x ========")

    val r = scala.util.Random
    val z = (for (i <- 1 to x) yield r.nextInt(2 * x)).toArray
    input = quickSort(z)
    
    println("Generating code snippet in /out/"+under+"-bs-"+x+".actual.scala")
    //exec("-bs-"+x, snippet.code)
    //snippet.precompile

    for(c <- 1 to y)
    {
        val ts1 = System.nanoTime() / 1000
        for(b <- 0 until x)
        {
            //secondSnippet(input(b))
            
            if(x == 40) { apply_40(input(b)) }
            else if (x == 400) { apply_400(input(b)) }
            else if (x == 4000) { apply_4000(input(b)) }
            else if (x == 40000) { apply_40000(input(b)) }
            
        }
        val ts2 = System.nanoTime() / 1000
        println("  Staged: Elapsed time: " + (ts2 - ts1) + " us")

        val tu1 = System.nanoTime() / 1000
        for(a <- 0 until x)
        {
            binarySearchUnstaged(input(a))
        }
        val tu2 = System.nanoTime() / 1000
        println("Unstaged: Elapsed time: " + (tu2 - tu1) + " us")
    }
  }

    def quickSort(xs: Array[Int]): Array[Int] = {
        if (xs.length <= 1) xs
        else {
            val pivot = xs(xs.length / 2)
            Array.concat(
                quickSort(xs filter (pivot >)),
                xs filter (pivot ==),
                quickSort(xs filter (pivot <)))
        }
    }

    def apply_40000(x0:Int): Int = {
        val x1 = input
        val x223 = {(x224:Int,x225:Int) =>
        // bs_17
        -1: Int
        }
        val x210 = {(x211:Int,x212:Int) =>
        // bs_16
        val x213 = x211
        val x214 = x212
        val x216 = x213 > x214
        val x242 = if (x216) {
            -1
        } else {
            val x217 = x214 - x213
            val x218 = x217 / 2
            val x219 = x213 + x218
            val x220 = x1(x219)
            val x221 = x220 == x0
            val x240 = if (x221) {
            x219
            } else {
            val x222 = x220 < x0
            val x238 = if (x222) {
                val x230 = x219 + 1
                val x232 = x223(x230,x214)
                x232
            } else {
                val x234 = x219 - 1
                val x236 = x223(x213,x234)
                x236
            }
            x238
            }
            x240
        }
        x242: Int
        }
        val x197 = {(x198:Int,x199:Int) =>
        // bs_15
        val x200 = x198
        val x201 = x199
        val x203 = x200 > x201
        val x256 = if (x203) {
            -1
        } else {
            val x204 = x201 - x200
            val x205 = x204 / 2
            val x206 = x200 + x205
            val x207 = x1(x206)
            val x208 = x207 == x0
            val x254 = if (x208) {
            x206
            } else {
            val x209 = x207 < x0
            val x252 = if (x209) {
                val x244 = x206 + 1
                val x246 = x210(x244,x201)
                x246
            } else {
                val x248 = x206 - 1
                val x250 = x210(x200,x248)
                x250
            }
            x252
            }
            x254
        }
        x256: Int
        }
        val x184 = {(x185:Int,x186:Int) =>
        // bs_14
        val x187 = x185
        val x188 = x186
        val x190 = x187 > x188
        val x270 = if (x190) {
            -1
        } else {
            val x191 = x188 - x187
            val x192 = x191 / 2
            val x193 = x187 + x192
            val x194 = x1(x193)
            val x195 = x194 == x0
            val x268 = if (x195) {
            x193
            } else {
            val x196 = x194 < x0
            val x266 = if (x196) {
                val x258 = x193 + 1
                val x260 = x197(x258,x188)
                x260
            } else {
                val x262 = x193 - 1
                val x264 = x197(x187,x262)
                x264
            }
            x266
            }
            x268
        }
        x270: Int
        }
        val x171 = {(x172:Int,x173:Int) =>
        // bs_13
        val x174 = x172
        val x175 = x173
        val x177 = x174 > x175
        val x284 = if (x177) {
            -1
        } else {
            val x178 = x175 - x174
            val x179 = x178 / 2
            val x180 = x174 + x179
            val x181 = x1(x180)
            val x182 = x181 == x0
            val x282 = if (x182) {
            x180
            } else {
            val x183 = x181 < x0
            val x280 = if (x183) {
                val x272 = x180 + 1
                val x274 = x184(x272,x175)
                x274
            } else {
                val x276 = x180 - 1
                val x278 = x184(x174,x276)
                x278
            }
            x280
            }
            x282
        }
        x284: Int
        }
        val x158 = {(x159:Int,x160:Int) =>
        // bs_12
        val x161 = x159
        val x162 = x160
        val x164 = x161 > x162
        val x298 = if (x164) {
            -1
        } else {
            val x165 = x162 - x161
            val x166 = x165 / 2
            val x167 = x161 + x166
            val x168 = x1(x167)
            val x169 = x168 == x0
            val x296 = if (x169) {
            x167
            } else {
            val x170 = x168 < x0
            val x294 = if (x170) {
                val x286 = x167 + 1
                val x288 = x171(x286,x162)
                x288
            } else {
                val x290 = x167 - 1
                val x292 = x171(x161,x290)
                x292
            }
            x294
            }
            x296
        }
        x298: Int
        }
        val x145 = {(x146:Int,x147:Int) =>
        // bs_11
        val x148 = x146
        val x149 = x147
        val x151 = x148 > x149
        val x312 = if (x151) {
            -1
        } else {
            val x152 = x149 - x148
            val x153 = x152 / 2
            val x154 = x148 + x153
            val x155 = x1(x154)
            val x156 = x155 == x0
            val x310 = if (x156) {
            x154
            } else {
            val x157 = x155 < x0
            val x308 = if (x157) {
                val x300 = x154 + 1
                val x302 = x158(x300,x149)
                x302
            } else {
                val x304 = x154 - 1
                val x306 = x158(x148,x304)
                x306
            }
            x308
            }
            x310
        }
        x312: Int
        }
        val x132 = {(x133:Int,x134:Int) =>
        // bs_10
        val x135 = x133
        val x136 = x134
        val x138 = x135 > x136
        val x326 = if (x138) {
            -1
        } else {
            val x139 = x136 - x135
            val x140 = x139 / 2
            val x141 = x135 + x140
            val x142 = x1(x141)
            val x143 = x142 == x0
            val x324 = if (x143) {
            x141
            } else {
            val x144 = x142 < x0
            val x322 = if (x144) {
                val x314 = x141 + 1
                val x316 = x145(x314,x136)
                x316
            } else {
                val x318 = x141 - 1
                val x320 = x145(x135,x318)
                x320
            }
            x322
            }
            x324
        }
        x326: Int
        }
        val x119 = {(x120:Int,x121:Int) =>
        // bs_9
        val x122 = x120
        val x123 = x121
        val x125 = x122 > x123
        val x340 = if (x125) {
            -1
        } else {
            val x126 = x123 - x122
            val x127 = x126 / 2
            val x128 = x122 + x127
            val x129 = x1(x128)
            val x130 = x129 == x0
            val x338 = if (x130) {
            x128
            } else {
            val x131 = x129 < x0
            val x336 = if (x131) {
                val x328 = x128 + 1
                val x330 = x132(x328,x123)
                x330
            } else {
                val x332 = x128 - 1
                val x334 = x132(x122,x332)
                x334
            }
            x336
            }
            x338
        }
        x340: Int
        }
        val x106 = {(x107:Int,x108:Int) =>
        // bs_8
        val x109 = x107
        val x110 = x108
        val x112 = x109 > x110
        val x354 = if (x112) {
            -1
        } else {
            val x113 = x110 - x109
            val x114 = x113 / 2
            val x115 = x109 + x114
            val x116 = x1(x115)
            val x117 = x116 == x0
            val x352 = if (x117) {
            x115
            } else {
            val x118 = x116 < x0
            val x350 = if (x118) {
                val x342 = x115 + 1
                val x344 = x119(x342,x110)
                x344
            } else {
                val x346 = x115 - 1
                val x348 = x119(x109,x346)
                x348
            }
            x350
            }
            x352
        }
        x354: Int
        }
        val x93 = {(x94:Int,x95:Int) =>
        // bs_7
        val x96 = x94
        val x97 = x95
        val x99 = x96 > x97
        val x368 = if (x99) {
            -1
        } else {
            val x100 = x97 - x96
            val x101 = x100 / 2
            val x102 = x96 + x101
            val x103 = x1(x102)
            val x104 = x103 == x0
            val x366 = if (x104) {
            x102
            } else {
            val x105 = x103 < x0
            val x364 = if (x105) {
                val x356 = x102 + 1
                val x358 = x106(x356,x97)
                x358
            } else {
                val x360 = x102 - 1
                val x362 = x106(x96,x360)
                x362
            }
            x364
            }
            x366
        }
        x368: Int
        }
        val x80 = {(x81:Int,x82:Int) =>
        // bs_6
        val x83 = x81
        val x84 = x82
        val x86 = x83 > x84
        val x382 = if (x86) {
            -1
        } else {
            val x87 = x84 - x83
            val x88 = x87 / 2
            val x89 = x83 + x88
            val x90 = x1(x89)
            val x91 = x90 == x0
            val x380 = if (x91) {
            x89
            } else {
            val x92 = x90 < x0
            val x378 = if (x92) {
                val x370 = x89 + 1
                val x372 = x93(x370,x84)
                x372
            } else {
                val x374 = x89 - 1
                val x376 = x93(x83,x374)
                x376
            }
            x378
            }
            x380
        }
        x382: Int
        }
        val x67 = {(x68:Int,x69:Int) =>
        // bs_5
        val x70 = x68
        val x71 = x69
        val x73 = x70 > x71
        val x396 = if (x73) {
            -1
        } else {
            val x74 = x71 - x70
            val x75 = x74 / 2
            val x76 = x70 + x75
            val x77 = x1(x76)
            val x78 = x77 == x0
            val x394 = if (x78) {
            x76
            } else {
            val x79 = x77 < x0
            val x392 = if (x79) {
                val x384 = x76 + 1
                val x386 = x80(x384,x71)
                x386
            } else {
                val x388 = x76 - 1
                val x390 = x80(x70,x388)
                x390
            }
            x392
            }
            x394
        }
        x396: Int
        }
        val x54 = {(x55:Int,x56:Int) =>
        // bs_4
        val x57 = x55
        val x58 = x56
        val x60 = x57 > x58
        val x410 = if (x60) {
            -1
        } else {
            val x61 = x58 - x57
            val x62 = x61 / 2
            val x63 = x57 + x62
            val x64 = x1(x63)
            val x65 = x64 == x0
            val x408 = if (x65) {
            x63
            } else {
            val x66 = x64 < x0
            val x406 = if (x66) {
                val x398 = x63 + 1
                val x400 = x67(x398,x58)
                x400
            } else {
                val x402 = x63 - 1
                val x404 = x67(x57,x402)
                x404
            }
            x406
            }
            x408
        }
        x410: Int
        }
        val x41 = {(x42:Int,x43:Int) =>
        // bs_3
        val x44 = x42
        val x45 = x43
        val x47 = x44 > x45
        val x424 = if (x47) {
            -1
        } else {
            val x48 = x45 - x44
            val x49 = x48 / 2
            val x50 = x44 + x49
            val x51 = x1(x50)
            val x52 = x51 == x0
            val x422 = if (x52) {
            x50
            } else {
            val x53 = x51 < x0
            val x420 = if (x53) {
                val x412 = x50 + 1
                val x414 = x54(x412,x45)
                x414
            } else {
                val x416 = x50 - 1
                val x418 = x54(x44,x416)
                x418
            }
            x420
            }
            x422
        }
        x424: Int
        }
        val x28 = {(x29:Int,x30:Int) =>
        // bs_2
        val x31 = x29
        val x32 = x30
        val x34 = x31 > x32
        val x438 = if (x34) {
            -1
        } else {
            val x35 = x32 - x31
            val x36 = x35 / 2
            val x37 = x31 + x36
            val x38 = x1(x37)
            val x39 = x38 == x0
            val x436 = if (x39) {
            x37
            } else {
            val x40 = x38 < x0
            val x434 = if (x40) {
                val x426 = x37 + 1
                val x428 = x41(x426,x32)
                x428
            } else {
                val x430 = x37 - 1
                val x432 = x41(x31,x430)
                x432
            }
            x434
            }
            x436
        }
        x438: Int
        }
        val x15 = {(x16:Int,x17:Int) =>
        // bs_1
        val x18 = x16
        val x19 = x17
        val x21 = x18 > x19
        val x452 = if (x21) {
            -1
        } else {
            val x22 = x19 - x18
            val x23 = x22 / 2
            val x24 = x18 + x23
            val x25 = x1(x24)
            val x26 = x25 == x0
            val x450 = if (x26) {
            x24
            } else {
            val x27 = x25 < x0
            val x448 = if (x27) {
                val x440 = x24 + 1
                val x442 = x28(x440,x19)
                x442
            } else {
                val x444 = x24 - 1
                val x446 = x28(x18,x444)
                x446
            }
            x448
            }
            x450
        }
        x452: Int
        }
        val x2 = {(x3:Int,x4:Int) =>
        // bs_0
        val x5 = x3
        val x6 = x4
        val x8 = x5 > x6
        val x466 = if (x8) {
            -1
        } else {
            val x9 = x6 - x5
            val x10 = x9 / 2
            val x11 = x5 + x10
            val x12 = x1(x11)
            val x13 = x12 == x0
            val x464 = if (x13) {
            x11
            } else {
            val x14 = x12 < x0
            val x462 = if (x14) {
                val x454 = x11 + 1
                val x456 = x15(x454,x6)
                x456
            } else {
                val x458 = x11 - 1
                val x460 = x15(x5,x458)
                x460
            }
            x462
            }
            x464
        }
        x466: Int
        }
        val x468 = x1.length
        val x469 = x468 - 1
        val x471 = x2(0,x469)
        x471
    }

    def apply_4000(x0:Int): Int = {
        val x1 = input
        val x171 = {(x172:Int,x173:Int) =>
        // bs_13
        -1: Int
        }
        val x158 = {(x159:Int,x160:Int) =>
        // bs_12
        val x161 = x159
        val x162 = x160
        val x164 = x161 > x162
        val x190 = if (x164) {
            -1
        } else {
            val x165 = x162 - x161
            val x166 = x165 / 2
            val x167 = x161 + x166
            val x168 = x1(x167)
            val x169 = x168 == x0
            val x188 = if (x169) {
            x167
            } else {
            val x170 = x168 < x0
            val x186 = if (x170) {
                val x178 = x167 + 1
                val x180 = x171(x178,x162)
                x180
            } else {
                val x182 = x167 - 1
                val x184 = x171(x161,x182)
                x184
            }
            x186
            }
            x188
        }
        x190: Int
        }
        val x145 = {(x146:Int,x147:Int) =>
        // bs_11
        val x148 = x146
        val x149 = x147
        val x151 = x148 > x149
        val x204 = if (x151) {
            -1
        } else {
            val x152 = x149 - x148
            val x153 = x152 / 2
            val x154 = x148 + x153
            val x155 = x1(x154)
            val x156 = x155 == x0
            val x202 = if (x156) {
            x154
            } else {
            val x157 = x155 < x0
            val x200 = if (x157) {
                val x192 = x154 + 1
                val x194 = x158(x192,x149)
                x194
            } else {
                val x196 = x154 - 1
                val x198 = x158(x148,x196)
                x198
            }
            x200
            }
            x202
        }
        x204: Int
        }
        val x132 = {(x133:Int,x134:Int) =>
        // bs_10
        val x135 = x133
        val x136 = x134
        val x138 = x135 > x136
        val x218 = if (x138) {
            -1
        } else {
            val x139 = x136 - x135
            val x140 = x139 / 2
            val x141 = x135 + x140
            val x142 = x1(x141)
            val x143 = x142 == x0
            val x216 = if (x143) {
            x141
            } else {
            val x144 = x142 < x0
            val x214 = if (x144) {
                val x206 = x141 + 1
                val x208 = x145(x206,x136)
                x208
            } else {
                val x210 = x141 - 1
                val x212 = x145(x135,x210)
                x212
            }
            x214
            }
            x216
        }
        x218: Int
        }
        val x119 = {(x120:Int,x121:Int) =>
        // bs_9
        val x122 = x120
        val x123 = x121
        val x125 = x122 > x123
        val x232 = if (x125) {
            -1
        } else {
            val x126 = x123 - x122
            val x127 = x126 / 2
            val x128 = x122 + x127
            val x129 = x1(x128)
            val x130 = x129 == x0
            val x230 = if (x130) {
            x128
            } else {
            val x131 = x129 < x0
            val x228 = if (x131) {
                val x220 = x128 + 1
                val x222 = x132(x220,x123)
                x222
            } else {
                val x224 = x128 - 1
                val x226 = x132(x122,x224)
                x226
            }
            x228
            }
            x230
        }
        x232: Int
        }
        val x106 = {(x107:Int,x108:Int) =>
        // bs_8
        val x109 = x107
        val x110 = x108
        val x112 = x109 > x110
        val x246 = if (x112) {
            -1
        } else {
            val x113 = x110 - x109
            val x114 = x113 / 2
            val x115 = x109 + x114
            val x116 = x1(x115)
            val x117 = x116 == x0
            val x244 = if (x117) {
            x115
            } else {
            val x118 = x116 < x0
            val x242 = if (x118) {
                val x234 = x115 + 1
                val x236 = x119(x234,x110)
                x236
            } else {
                val x238 = x115 - 1
                val x240 = x119(x109,x238)
                x240
            }
            x242
            }
            x244
        }
        x246: Int
        }
        val x93 = {(x94:Int,x95:Int) =>
        // bs_7
        val x96 = x94
        val x97 = x95
        val x99 = x96 > x97
        val x260 = if (x99) {
            -1
        } else {
            val x100 = x97 - x96
            val x101 = x100 / 2
            val x102 = x96 + x101
            val x103 = x1(x102)
            val x104 = x103 == x0
            val x258 = if (x104) {
            x102
            } else {
            val x105 = x103 < x0
            val x256 = if (x105) {
                val x248 = x102 + 1
                val x250 = x106(x248,x97)
                x250
            } else {
                val x252 = x102 - 1
                val x254 = x106(x96,x252)
                x254
            }
            x256
            }
            x258
        }
        x260: Int
        }
        val x80 = {(x81:Int,x82:Int) =>
        // bs_6
        val x83 = x81
        val x84 = x82
        val x86 = x83 > x84
        val x274 = if (x86) {
            -1
        } else {
            val x87 = x84 - x83
            val x88 = x87 / 2
            val x89 = x83 + x88
            val x90 = x1(x89)
            val x91 = x90 == x0
            val x272 = if (x91) {
            x89
            } else {
            val x92 = x90 < x0
            val x270 = if (x92) {
                val x262 = x89 + 1
                val x264 = x93(x262,x84)
                x264
            } else {
                val x266 = x89 - 1
                val x268 = x93(x83,x266)
                x268
            }
            x270
            }
            x272
        }
        x274: Int
        }
        val x67 = {(x68:Int,x69:Int) =>
        // bs_5
        val x70 = x68
        val x71 = x69
        val x73 = x70 > x71
        val x288 = if (x73) {
            -1
        } else {
            val x74 = x71 - x70
            val x75 = x74 / 2
            val x76 = x70 + x75
            val x77 = x1(x76)
            val x78 = x77 == x0
            val x286 = if (x78) {
            x76
            } else {
            val x79 = x77 < x0
            val x284 = if (x79) {
                val x276 = x76 + 1
                val x278 = x80(x276,x71)
                x278
            } else {
                val x280 = x76 - 1
                val x282 = x80(x70,x280)
                x282
            }
            x284
            }
            x286
        }
        x288: Int
        }
        val x54 = {(x55:Int,x56:Int) =>
        // bs_4
        val x57 = x55
        val x58 = x56
        val x60 = x57 > x58
        val x302 = if (x60) {
            -1
        } else {
            val x61 = x58 - x57
            val x62 = x61 / 2
            val x63 = x57 + x62
            val x64 = x1(x63)
            val x65 = x64 == x0
            val x300 = if (x65) {
            x63
            } else {
            val x66 = x64 < x0
            val x298 = if (x66) {
                val x290 = x63 + 1
                val x292 = x67(x290,x58)
                x292
            } else {
                val x294 = x63 - 1
                val x296 = x67(x57,x294)
                x296
            }
            x298
            }
            x300
        }
        x302: Int
        }
        val x41 = {(x42:Int,x43:Int) =>
        // bs_3
        val x44 = x42
        val x45 = x43
        val x47 = x44 > x45
        val x316 = if (x47) {
            -1
        } else {
            val x48 = x45 - x44
            val x49 = x48 / 2
            val x50 = x44 + x49
            val x51 = x1(x50)
            val x52 = x51 == x0
            val x314 = if (x52) {
            x50
            } else {
            val x53 = x51 < x0
            val x312 = if (x53) {
                val x304 = x50 + 1
                val x306 = x54(x304,x45)
                x306
            } else {
                val x308 = x50 - 1
                val x310 = x54(x44,x308)
                x310
            }
            x312
            }
            x314
        }
        x316: Int
        }
        val x28 = {(x29:Int,x30:Int) =>
        // bs_2
        val x31 = x29
        val x32 = x30
        val x34 = x31 > x32
        val x330 = if (x34) {
            -1
        } else {
            val x35 = x32 - x31
            val x36 = x35 / 2
            val x37 = x31 + x36
            val x38 = x1(x37)
            val x39 = x38 == x0
            val x328 = if (x39) {
            x37
            } else {
            val x40 = x38 < x0
            val x326 = if (x40) {
                val x318 = x37 + 1
                val x320 = x41(x318,x32)
                x320
            } else {
                val x322 = x37 - 1
                val x324 = x41(x31,x322)
                x324
            }
            x326
            }
            x328
        }
        x330: Int
        }
        val x15 = {(x16:Int,x17:Int) =>
        // bs_1
        val x18 = x16
        val x19 = x17
        val x21 = x18 > x19
        val x344 = if (x21) {
            -1
        } else {
            val x22 = x19 - x18
            val x23 = x22 / 2
            val x24 = x18 + x23
            val x25 = x1(x24)
            val x26 = x25 == x0
            val x342 = if (x26) {
            x24
            } else {
            val x27 = x25 < x0
            val x340 = if (x27) {
                val x332 = x24 + 1
                val x334 = x28(x332,x19)
                x334
            } else {
                val x336 = x24 - 1
                val x338 = x28(x18,x336)
                x338
            }
            x340
            }
            x342
        }
        x344: Int
        }
        val x2 = {(x3:Int,x4:Int) =>
        // bs_0
        val x5 = x3
        val x6 = x4
        val x8 = x5 > x6
        val x358 = if (x8) {
            -1
        } else {
            val x9 = x6 - x5
            val x10 = x9 / 2
            val x11 = x5 + x10
            val x12 = x1(x11)
            val x13 = x12 == x0
            val x356 = if (x13) {
            x11
            } else {
            val x14 = x12 < x0
            val x354 = if (x14) {
                val x346 = x11 + 1
                val x348 = x15(x346,x6)
                x348
            } else {
                val x350 = x11 - 1
                val x352 = x15(x5,x350)
                x352
            }
            x354
            }
            x356
        }
        x358: Int
        }
        val x360 = x1.length
        val x361 = x360 - 1
        val x363 = x2(0,x361)
        x363
    }

    def apply_400(x0:Int): Int = {
        val x1 = input
        val x132 = {(x133:Int,x134:Int) =>
        // bs_10
        -1: Int
        }
        val x119 = {(x120:Int,x121:Int) =>
        // bs_9
        val x122 = x120
        val x123 = x121
        val x125 = x122 > x123
        val x151 = if (x125) {
            -1
        } else {
            val x126 = x123 - x122
            val x127 = x126 / 2
            val x128 = x122 + x127
            val x129 = x1(x128)
            val x130 = x129 == x0
            val x149 = if (x130) {
            x128
            } else {
            val x131 = x129 < x0
            val x147 = if (x131) {
                val x139 = x128 + 1
                val x141 = x132(x139,x123)
                x141
            } else {
                val x143 = x128 - 1
                val x145 = x132(x122,x143)
                x145
            }
            x147
            }
            x149
        }
        x151: Int
        }
        val x106 = {(x107:Int,x108:Int) =>
        // bs_8
        val x109 = x107
        val x110 = x108
        val x112 = x109 > x110
        val x165 = if (x112) {
            -1
        } else {
            val x113 = x110 - x109
            val x114 = x113 / 2
            val x115 = x109 + x114
            val x116 = x1(x115)
            val x117 = x116 == x0
            val x163 = if (x117) {
            x115
            } else {
            val x118 = x116 < x0
            val x161 = if (x118) {
                val x153 = x115 + 1
                val x155 = x119(x153,x110)
                x155
            } else {
                val x157 = x115 - 1
                val x159 = x119(x109,x157)
                x159
            }
            x161
            }
            x163
        }
        x165: Int
        }
        val x93 = {(x94:Int,x95:Int) =>
        // bs_7
        val x96 = x94
        val x97 = x95
        val x99 = x96 > x97
        val x179 = if (x99) {
            -1
        } else {
            val x100 = x97 - x96
            val x101 = x100 / 2
            val x102 = x96 + x101
            val x103 = x1(x102)
            val x104 = x103 == x0
            val x177 = if (x104) {
            x102
            } else {
            val x105 = x103 < x0
            val x175 = if (x105) {
                val x167 = x102 + 1
                val x169 = x106(x167,x97)
                x169
            } else {
                val x171 = x102 - 1
                val x173 = x106(x96,x171)
                x173
            }
            x175
            }
            x177
        }
        x179: Int
        }
        val x80 = {(x81:Int,x82:Int) =>
        // bs_6
        val x83 = x81
        val x84 = x82
        val x86 = x83 > x84
        val x193 = if (x86) {
            -1
        } else {
            val x87 = x84 - x83
            val x88 = x87 / 2
            val x89 = x83 + x88
            val x90 = x1(x89)
            val x91 = x90 == x0
            val x191 = if (x91) {
            x89
            } else {
            val x92 = x90 < x0
            val x189 = if (x92) {
                val x181 = x89 + 1
                val x183 = x93(x181,x84)
                x183
            } else {
                val x185 = x89 - 1
                val x187 = x93(x83,x185)
                x187
            }
            x189
            }
            x191
        }
        x193: Int
        }
        val x67 = {(x68:Int,x69:Int) =>
        // bs_5
        val x70 = x68
        val x71 = x69
        val x73 = x70 > x71
        val x207 = if (x73) {
            -1
        } else {
            val x74 = x71 - x70
            val x75 = x74 / 2
            val x76 = x70 + x75
            val x77 = x1(x76)
            val x78 = x77 == x0
            val x205 = if (x78) {
            x76
            } else {
            val x79 = x77 < x0
            val x203 = if (x79) {
                val x195 = x76 + 1
                val x197 = x80(x195,x71)
                x197
            } else {
                val x199 = x76 - 1
                val x201 = x80(x70,x199)
                x201
            }
            x203
            }
            x205
        }
        x207: Int
        }
        val x54 = {(x55:Int,x56:Int) =>
        // bs_4
        val x57 = x55
        val x58 = x56
        val x60 = x57 > x58
        val x221 = if (x60) {
            -1
        } else {
            val x61 = x58 - x57
            val x62 = x61 / 2
            val x63 = x57 + x62
            val x64 = x1(x63)
            val x65 = x64 == x0
            val x219 = if (x65) {
            x63
            } else {
            val x66 = x64 < x0
            val x217 = if (x66) {
                val x209 = x63 + 1
                val x211 = x67(x209,x58)
                x211
            } else {
                val x213 = x63 - 1
                val x215 = x67(x57,x213)
                x215
            }
            x217
            }
            x219
        }
        x221: Int
        }
        val x41 = {(x42:Int,x43:Int) =>
        // bs_3
        val x44 = x42
        val x45 = x43
        val x47 = x44 > x45
        val x235 = if (x47) {
            -1
        } else {
            val x48 = x45 - x44
            val x49 = x48 / 2
            val x50 = x44 + x49
            val x51 = x1(x50)
            val x52 = x51 == x0
            val x233 = if (x52) {
            x50
            } else {
            val x53 = x51 < x0
            val x231 = if (x53) {
                val x223 = x50 + 1
                val x225 = x54(x223,x45)
                x225
            } else {
                val x227 = x50 - 1
                val x229 = x54(x44,x227)
                x229
            }
            x231
            }
            x233
        }
        x235: Int
        }
        val x28 = {(x29:Int,x30:Int) =>
        // bs_2
        val x31 = x29
        val x32 = x30
        val x34 = x31 > x32
        val x249 = if (x34) {
            -1
        } else {
            val x35 = x32 - x31
            val x36 = x35 / 2
            val x37 = x31 + x36
            val x38 = x1(x37)
            val x39 = x38 == x0
            val x247 = if (x39) {
            x37
            } else {
            val x40 = x38 < x0
            val x245 = if (x40) {
                val x237 = x37 + 1
                val x239 = x41(x237,x32)
                x239
            } else {
                val x241 = x37 - 1
                val x243 = x41(x31,x241)
                x243
            }
            x245
            }
            x247
        }
        x249: Int
        }
        val x15 = {(x16:Int,x17:Int) =>
        // bs_1
        val x18 = x16
        val x19 = x17
        val x21 = x18 > x19
        val x263 = if (x21) {
            -1
        } else {
            val x22 = x19 - x18
            val x23 = x22 / 2
            val x24 = x18 + x23
            val x25 = x1(x24)
            val x26 = x25 == x0
            val x261 = if (x26) {
            x24
            } else {
            val x27 = x25 < x0
            val x259 = if (x27) {
                val x251 = x24 + 1
                val x253 = x28(x251,x19)
                x253
            } else {
                val x255 = x24 - 1
                val x257 = x28(x18,x255)
                x257
            }
            x259
            }
            x261
        }
        x263: Int
        }
        val x2 = {(x3:Int,x4:Int) =>
        // bs_0
        val x5 = x3
        val x6 = x4
        val x8 = x5 > x6
        val x277 = if (x8) {
            -1
        } else {
            val x9 = x6 - x5
            val x10 = x9 / 2
            val x11 = x5 + x10
            val x12 = x1(x11)
            val x13 = x12 == x0
            val x275 = if (x13) {
            x11
            } else {
            val x14 = x12 < x0
            val x273 = if (x14) {
                val x265 = x11 + 1
                val x267 = x15(x265,x6)
                x267
            } else {
                val x269 = x11 - 1
                val x271 = x15(x5,x269)
                x271
            }
            x273
            }
            x275
        }
        x277: Int
        }
        val x279 = x1.length
        val x280 = x279 - 1
        val x282 = x2(0,x280)
        x282
    }

    def apply_40(x0:Int): Int = {
        val x1 = input
        val x93 = {(x94:Int,x95:Int) =>
        // bs_7
        -1: Int
        }
        val x80 = {(x81:Int,x82:Int) =>
        // bs_6
        val x83 = x81
        val x84 = x82
        val x86 = x83 > x84
        val x112 = if (x86) {
            -1
        } else {
            val x87 = x84 - x83
            val x88 = x87 / 2
            val x89 = x83 + x88
            val x90 = x1(x89)
            val x91 = x90 == x0
            val x110 = if (x91) {
            x89
            } else {
            val x92 = x90 < x0
            val x108 = if (x92) {
                val x100 = x89 + 1
                val x102 = x93(x100,x84)
                x102
            } else {
                val x104 = x89 - 1
                val x106 = x93(x83,x104)
                x106
            }
            x108
            }
            x110
        }
        x112: Int
        }
        val x67 = {(x68:Int,x69:Int) =>
        // bs_5
        val x70 = x68
        val x71 = x69
        val x73 = x70 > x71
        val x126 = if (x73) {
            -1
        } else {
            val x74 = x71 - x70
            val x75 = x74 / 2
            val x76 = x70 + x75
            val x77 = x1(x76)
            val x78 = x77 == x0
            val x124 = if (x78) {
            x76
            } else {
            val x79 = x77 < x0
            val x122 = if (x79) {
                val x114 = x76 + 1
                val x116 = x80(x114,x71)
                x116
            } else {
                val x118 = x76 - 1
                val x120 = x80(x70,x118)
                x120
            }
            x122
            }
            x124
        }
        x126: Int
        }
        val x54 = {(x55:Int,x56:Int) =>
        // bs_4
        val x57 = x55
        val x58 = x56
        val x60 = x57 > x58
        val x140 = if (x60) {
            -1
        } else {
            val x61 = x58 - x57
            val x62 = x61 / 2
            val x63 = x57 + x62
            val x64 = x1(x63)
            val x65 = x64 == x0
            val x138 = if (x65) {
            x63
            } else {
            val x66 = x64 < x0
            val x136 = if (x66) {
                val x128 = x63 + 1
                val x130 = x67(x128,x58)
                x130
            } else {
                val x132 = x63 - 1
                val x134 = x67(x57,x132)
                x134
            }
            x136
            }
            x138
        }
        x140: Int
        }
        val x41 = {(x42:Int,x43:Int) =>
        // bs_3
        val x44 = x42
        val x45 = x43
        val x47 = x44 > x45
        val x154 = if (x47) {
            -1
        } else {
            val x48 = x45 - x44
            val x49 = x48 / 2
            val x50 = x44 + x49
            val x51 = x1(x50)
            val x52 = x51 == x0
            val x152 = if (x52) {
            x50
            } else {
            val x53 = x51 < x0
            val x150 = if (x53) {
                val x142 = x50 + 1
                val x144 = x54(x142,x45)
                x144
            } else {
                val x146 = x50 - 1
                val x148 = x54(x44,x146)
                x148
            }
            x150
            }
            x152
        }
        x154: Int
        }
        val x28 = {(x29:Int,x30:Int) =>
        // bs_2
        val x31 = x29
        val x32 = x30
        val x34 = x31 > x32
        val x168 = if (x34) {
            -1
        } else {
            val x35 = x32 - x31
            val x36 = x35 / 2
            val x37 = x31 + x36
            val x38 = x1(x37)
            val x39 = x38 == x0
            val x166 = if (x39) {
            x37
            } else {
            val x40 = x38 < x0
            val x164 = if (x40) {
                val x156 = x37 + 1
                val x158 = x41(x156,x32)
                x158
            } else {
                val x160 = x37 - 1
                val x162 = x41(x31,x160)
                x162
            }
            x164
            }
            x166
        }
        x168: Int
        }
        val x15 = {(x16:Int,x17:Int) =>
        // bs_1
        val x18 = x16
        val x19 = x17
        val x21 = x18 > x19
        val x182 = if (x21) {
            -1
        } else {
            val x22 = x19 - x18
            val x23 = x22 / 2
            val x24 = x18 + x23
            val x25 = x1(x24)
            val x26 = x25 == x0
            val x180 = if (x26) {
            x24
            } else {
            val x27 = x25 < x0
            val x178 = if (x27) {
                val x170 = x24 + 1
                val x172 = x28(x170,x19)
                x172
            } else {
                val x174 = x24 - 1
                val x176 = x28(x18,x174)
                x176
            }
            x178
            }
            x180
        }
        x182: Int
        }
        val x2 = {(x3:Int,x4:Int) =>
        // bs_0
        val x5 = x3
        val x6 = x4
        val x8 = x5 > x6
        val x196 = if (x8) {
            -1
        } else {
            val x9 = x6 - x5
            val x10 = x9 / 2
            val x11 = x5 + x10
            val x12 = x1(x11)
            val x13 = x12 == x0
            val x194 = if (x13) {
            x11
            } else {
            val x14 = x12 < x0
            val x192 = if (x14) {
                val x184 = x11 + 1
                val x186 = x15(x184,x6)
                x186
            } else {
                val x188 = x11 - 1
                val x190 = x15(x5,x188)
                x190
            }
            x192
            }
            x194
        }
        x196: Int
        }
        val x198 = x1.length
        val x199 = x198 - 1
        val x201 = x2(0,x199)
        x201
    }
}