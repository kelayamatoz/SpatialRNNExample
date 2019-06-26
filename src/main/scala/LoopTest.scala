import spatial.dsl._

@spatial object LoopTest extends SpatialApp {
  def main(args: Array[String]): Unit = {
    type T = Int8

    val n0 = I32(128)
    val n1 = I32(256)
    val n2 = I32(128)
    val n3 = I32(64)

    val nInputDRAMs = 2
    val baseStride: I32 = 1.to[I32]
    val baseAddr: I32 = 0.to[I32]

    def getDRAM: DRAM4[T] = {
      val testData: Tensor4[T] = Tensor4.tabulate[T](n0, n1, n2, n3)((i, j, k, l) => random[T](1.to[T]))
      val tdDRAM: DRAM4[T] = DRAM[T](n0, n1, n2, n3)
      setMem(tdDRAM, testData)

      tdDRAM
    }

    val ins: List[DRAM4[T]] = List.tabulate(nInputDRAMs)(_ => getDRAM)
    val outs: DRAM4[T] = DRAM[T](n0, n1, n2, n3)

    Accel {
      val mems = List.tabulate[SRAM4[T]](nInputDRAMs)(_ => SRAM[T](n0, n1, n2, n3))
      (mems zip ins).foreach{ case (mem, dram) => mem load dram }

      val outMemBuf: SRAM4[T] = SRAM[T](n0, n1, n2, n3)
      val outMem: SRAM4[T] = SRAM[T](n0, n1, n2, n3)


      Foreach (n0 by baseStride) { i0 =>
        Foreach (n1 by baseStride) { i1 =>
          Foreach( n3 by baseStride) { i3 =>
            outMemBuf(i0, i1, baseAddr, i3) = List.tabulate(nInputDRAMs){
              case i =>
                val m: SRAM4[T] = mems(i)
                m(i0, i1, baseAddr, i3)
            }.sumTree
          }
        }

        Foreach (n2 by baseStride) { i2 =>
          Foreach (n3 by baseStride) { i3 =>
            outMem(i0, baseAddr, i2, i3) = List.tabulate(nInputDRAMs){
              case i =>
                val m: SRAM4[T] = mems(i)
                m(i0, baseAddr, i2, i3)
            }.sumTree + outMemBuf(i0, baseAddr, i2, i3)
          }
        }
      }

      outs store outMem
    }

    val result = getTensor4(outs)
    printTensor4(result)
  }
}
