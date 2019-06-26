package FloatTests

import spatial.dsl._

@spatial object TestEnable_2Ok extends SpatialApp {
  //@virtualize
  def main(args: Array[String]): Unit = {
    // Declare SW-HW interface vals
    type T = FixPt[TRUE, _16, _0]

    val x = ArgIn[T]
    val y = ArgOut[T]
    val N = args(0).to[T]

    val d0: DRAM1[T] = DRAM[T](16)
    val d1: DRAM1[T] = DRAM[T](16)
    val d2: DRAM1[T] = DRAM[T](16)
    //val d3 = DRAM[T](16)

    val data0 = Array.tabulate[T](16.to[I32])(i => i.to[T])
    val data1 = Array.tabulate[T](16.to[I32])(i => i.to[T])
    val data2 = Array.tabulate[T](16.to[I32])(i => i.to[T])
    //val data3 = Array.tabulate[T](16)(i => i.to[T])
    //val data = Array.fill[T](16)(1)
    setMem(d0, data0)
    setMem(d1, data1)
    setMem(d2, data2)
    // setMem(d3,data3)
    setArg(x, N)

//print after init

    printArray(getMem(d0), "d0_init = ")
    printArray(getMem(d1), "d1_init = ")
    printArray(getMem(d2), "d2_init = ")
    //printArray(getMem(d3), "d3_init = ")

    // Create HW accelerator
    Accel {
      val s0: SRAM1[T] = SRAM[T](16)
      val s1: SRAM1[T] = SRAM[T](16)
      val s2: SRAM1[T] = SRAM[T](16)
      // val s3 = SRAM[T](16)

      s0 load d0
      s1 load d1
      s2 load d2
      // s3 load d3

      y := x.value + s0(7.to[I32]) + s1(7.to[I32]) + s2(7.to[I32])
      //     y := Reduce(Reg[T](0))(16 by 1){i =>
      //        s0(i) + s1(i) + s2(i) + s3(i)
      //     }{_+_}

      //     y := y + x

      d0 store s0
      d1 store s1
      d2 store s2
      //d3 store s3
    }
    //val dout = getMem(d)
//print after accel
    printArray(getMem(d0), "d0 = ")
    printArray(getMem(d1), "d1 = ")
    printArray(getMem(d2), "d2 = ")
    //printArray(getMem(d3), "d3 = ")

    val re = getArg(y)
    val gold = getArg(x) + data0(7.to[I32]) + data1(7.to[I32]) + data2(7.to[I32])
    val chksum = (re - gold) == 0.to[T]
    println("argout = " + re + ", gold = " + gold + ", chksum = " + chksum)
  }
}
