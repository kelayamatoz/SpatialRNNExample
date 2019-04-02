import spatial.dsl._

@spatial
object AFGEMM extends SpatialApp {
  def main(args: Array[String]): Unit = {
    type T = FixPt[TRUE, _16, _16]
    // Try programmable types...
    // Func sim params
    val funcSim: scala.Boolean = true // synth: false
    val defaultPar = 16.to[I32] // synth: 16
    val basePar = 1.to[I32]

    // par
    val defaultLdSt = 1.to[I32]
    val kPar = defaultPar
    val nPar = defaultPar
    val ldPar = defaultLdSt
    val stPar = defaultLdSt

    // Problem sizes, unstaged
    val testSizeU: scala.Int = 4 * 16
    val lenAU: scala.Int = 16 * testSizeU
    val lenBU: scala.Int = 16 * 16 * testSizeU
    val lenCU: scala.Int = 16 * testSizeU

    // Const
    val baseAddr = 1.to[I32]
    val baseStride = 1.to[I32]

    // Problem sizes, staged
    val lenA = I32.apply(lenAU)
    val lenB = I32.apply(lenBU)
    val lenC = I32.apply(lenCU)

    // cmd
    val nCmds = 9.to[Int]
    val cArgOut = ArgOut[T]
    val m = 16.to[I32]
    val n = 16.to[I32]
    val k = 16.to[I32]
    val srcAddr = 0.to[I32]
    val weightAddr = 0.to[I32]
    val dstAddr = 0.to[I32]
    val srcStep = 64.to[I32]
    val weightStep = 64.to[I32]
    val dstStep = 64.to[I32]

    def genIdxes(range: Int): Array[Int] = Array.tabulate[Int](range){ i => i }

    def getSubMat(srcMat: Array[T], srcAddr: Int): Array[T] = {
      val idx = genIdxes(srcMat.length)
      val idxes = idx.filter(_ >= srcAddr)
      idxes.map(index => srcMat(index))
    }

    def printMem(mem: SRAM1[T], dim0: Int, name: java.lang.String): Unit = {
      print("========= ")
      print(name)
      println("=========")
      Foreach (dim0 by 16.to[I32]) { iLen =>
        Foreach (16.to[I32] by baseStride) { iEle =>
          val idx = iLen + iEle
          print(mem(idx)); print(", ")
        }
        println("")
      }
      println("============================")
    }

    def verifyArr(srcArr: Array[T], dstArr: Array[T],
                  m: Int, n: Int, k: Int,
                  srcStep: Int, weightStep: Int, dstStep: Int): Unit = {
      genIdxes(m).foreach(i =>
        genIdxes(n).foreach(j =>
        {
          val idx = i * dstStep / 4.to[I32] + j.to[I32]
          print("srcArr(idx) = ")
          print(srcArr(idx))
          print(", ")
          print("dstArr(idx) = ")
          print(dstArr(idx))
          print(", diff = ")
          print(abs(srcArr(idx) - dstArr(idx)))
          print(", ")
        }
        )
      )
    }


    val dataA = Array.tabulate[T](lenA){ i => i.to[T] }
    val dataB = Array.tabulate[T](lenB){ i => i.to[T] }
    val dataC = Array.empty[T](lenC)

    val aDRAM = DRAM[T](lenA)
    val bDRAM = DRAM[T](lenB)
    val cDRAM = DRAM[T](lenC)
    if (funcSim) {
      setMem(aDRAM, dataA)
      setMem(bDRAM, dataB)
    }

    // Verify
    val matA = getSubMat(dataA, srcAddr)
    val matB = getSubMat(dataB, weightAddr)
    val mIdxes = genIdxes(m)
    val nIdxes = genIdxes(n)
    val kIdxes = genIdxes(k)

    // Update dataC based on the given formula
    mIdxes.foreach(i =>
      nIdxes.foreach(j =>
        kIdxes.foreach(p =>
          if (p == 0) {
            dataC(i * dstStep / 4.to[I32] + j) =
              matA(i * srcStep / 4.to[I32] + p) *
                matB(p * weightStep / 4.to[I32] + j)
          } else {
            val dataCIdx = i * dstStep / 4.to[I32] + j
            dataC(dataCIdx) =
              dataC(dataCIdx) + matA(i * srcStep / 4.to[I32] + p) *
                matB(p * weightStep / 4.to[I32] + j)
          }
        )
      )
    )

    def getRandSeq(len: scala.Int): Seq[T] = scala.Seq.tabulate[T](len)(_ => random[T](1.to[T]))

    def getLUTFromSeq(seq: Seq[T]): LUT1[T] = {
      LUT.apply[T](seq.length)(seq: _*)
    }

    val matAData = getRandSeq(lenAU)
    val matBData = getRandSeq(lenBU)

    Accel {
//      val matrixA = getLUTFromSeq(matAData)
//      val matrixB = getLUTFromSeq(matBData)
      val matrixA = SRAM[T](lenA)
      val matrixB = SRAM[T](lenB)
      val matrixC = SRAM[T](lenC)

      val cmdLUT = LUT[I32](9.toInt)(
        m, n, k,
        srcAddr, weightAddr, dstAddr,
        srcStep, weightStep, dstStep
      )
      val cmdFIFO = FIFO[I32](nCmds.to[I32])

      // enque CMD FIFO
      Sequential.Foreach (nCmds by 1.to[I32]) { iCMD =>
        val cmd = cmdLUT(iCMD)
        if (funcSim) {
          println(cmd)
        }
        cmdFIFO.enq(cmd)
      }

      // AXB
      Pipe {
        val cmdRF = RegFile[I32](nCmds)
        Sequential.Foreach (nCmds by baseStride) { iRF =>
          cmdRF(iRF) = cmdFIFO.deq().to[I32]
        }

        // decode
        val m = cmdRF(0.to[I32])
        val k = cmdRF(1.to[I32])
        val n = cmdRF(2.to[I32])
        val srcAddr = cmdRF(3.to[I32])
        val weightAddr = cmdRF(4.to[I32])
        val dstAddr = cmdRF(5.to[I32])
        val srcStep = cmdRF(6.to[I32])
        val weightStep = cmdRF(7.to[I32])
        val dstStep = cmdRF(8.to[I32])

        Foreach (m by baseStride par basePar) { i =>
          Foreach (n by baseStride par defaultPar) { j =>
            val cIdx = i * dstStep / 4.to[I32] + j + dstAddr

            matrixC(cIdx) = Reduce (Reg[T](0.to[T]))(
              k by baseStride par defaultPar
            ) { p =>
              val aIdx = i * srcStep / 4.to[I32] + p + srcAddr
              val aEle = matrixA(aIdx)
              val bIdx = p * weightStep / 4.to[I32] + j + weightAddr
              val bEle = matrixB(bIdx)
              aEle * bEle
            }{_+_}.value

            if (funcSim) {
              // Just init 1 store for verifying functional correctness
              cDRAM(baseAddr::lenC) store matrixC
            }
          }
        }

        cArgOut := matrixC(0.to[I32])
      }
    }

    val result = getArg(cArgOut)
    if (funcSim) {
      val resultC = getMem(cDRAM)
      verifyArr(dataC, resultC, m, n, k, srcStep, weightStep, dstStep)
    }
    println(result)
  }
}
