import spatial.dsl._

@spatial
object RNN extends SpatialApp {
  def main(args: Array[String]): Unit = {
    // This is describing the LSTM dataflow
    type lowT = FixPt[TRUE, _4, _4]
    type highT = FixPt[TRUE, _16, _16]

    // TODO: how to set these non-linear functions?
    val tanh: highT => highT = x => {
      // 0.0001616 + 0.86635 * x - 0.00042018 * x^2 - 0.13333 * x^3
      val yReg: Reg[highT] = Reg[highT](0.to[highT])
      Pipe {
        val x2: highT = x * x
        val x3: highT = x2 * x
        val x1Term: highT = x * 0.86635.to[highT]
        val x2Term: highT = (-0.00042018).to[highT] * x2
        val x3Term: highT = (-0.13333).to[highT] * x3
        val y: highT = 0.0001616.to[highT] + x1Term + x2Term + x3Term
        yReg := y
      }
      yReg.value
    }

    val activation: highT => highT = x => {
      // 5-piece activation function
      val yReg: Reg[highT] = Reg[highT](0.to[highT])
      Pipe {
        val up = 2.5.to[highT]
        val lo = -2.5.to[highT]
        val posMid = 0.5.to[highT]
        val negMid = -0.5.to[highT]
        val bi = 0.375.to[highT]
        val divIn = x / 4.to[highT]

        val upMidLin = divIn - bi // (-2.5 ~ -0.5, x / 4 - 0.375)
        val loMidLin = -divIn + bi // (0.5 ~ 2.5, x / 4 + 0.375)
        val upLin = 1.to[highT]
        val loLin = -1.to[highT]

        val upCond = x > up
        val upMidCond = (posMid < x) && (x <= up)
        val loMidCond = (lo < x) && (x <= negMid)
        val loCond = x <= lo
        val y = mux(upCond,
          upLin,
          mux(upMidCond,
            upMidLin,
            mux(loMidCond, loMidLin, mux(loCond, loLin, x))))
        yReg := y
      }
      yReg.value
    }

    val activationI: highT => highT = activation
    val activationJ: highT => highT = tanh
    val activationF: highT => highT = activation
    val activationO: highT => highT = activation

    // TODO: what should be the parallelization and vectorization pars?
    val hu = 1
    val ru = 4
    val rv = 16

    // problem-specific params
    // val batchSize = 1
    val nHiddenUnits = 128
    val nFeatures = 128
    val nTimeSteps = 1
    val nGates = 4

    val ijfoDRAMs: scala.Array[DRAM2[lowT]] = scala.Array.tabulate(nGates) {
      _ =>
        val re: DRAM2[lowT] = DRAM[lowT](nHiddenUnits, nFeatures + nHiddenUnits)
        re
    }
    val ijfoData: scala.Array[Matrix[lowT]] = scala.Array.tabulate(nGates) {
      _ =>
        val re: Matrix[lowT] = Matrix.tabulate[lowT](
          nHiddenUnits.to[I32],
          (nFeatures + nHiddenUnits).to[I32]
        ) { (_, _) =>
          random[lowT](1.to[lowT])
        }
        re
    }
    ijfoDRAMs zip ijfoData foreach {
      case (mem, data) =>
        setMem(mem, data)
    }

    def getVecDRAM(size: I32): DRAM1[lowT] = {
      val re = DRAM[lowT](size)
      val reData = Array.tabulate[lowT](size)(_ => random[lowT](1.to[lowT]))
      setMem(re, reData)
      re
    }

    val cInitDRAM = getVecDRAM(nHiddenUnits.to[I32])
    val xhDRAM = getVecDRAM((nFeatures + nHiddenUnits).to[I32])

    val biasesDRAM: scala.Array[DRAM1[lowT]] = scala.Array.tabulate(nGates) {
      _ =>
        val re: DRAM1[lowT] = getVecDRAM(nHiddenUnits.to[I32])
        re
    }

    val xhResultDRAM: DRAM1[lowT] = DRAM[lowT](nHiddenUnits + nFeatures)
    val cResultDRAM: DRAM1[lowT] = DRAM[lowT](nHiddenUnits)

    Accel {
      // Load all gates weights and x, c, h data in one shot
      val ijfoMems: scala.Array[SRAM2[lowT]] = scala.Array.tabulate(nGates) {
        _ =>
          val re: SRAM2[lowT] =
            SRAM[lowT](nHiddenUnits, nFeatures + nHiddenUnits)
          re
      }
      ijfoMems zip ijfoDRAMs foreach {
        case (sram, dram) =>
          sram load dram(
            0.to[I32] :: nHiddenUnits.to[I32],
            0.to[I32] :: (nFeatures + nHiddenUnits).to[I32]
          )
      }

      val biasesMems: scala.Array[SRAM1[lowT]] = scala.Array.tabulate(nGates) {
        _ =>
          val re: SRAM1[lowT] = SRAM[lowT](nHiddenUnits)
          re
      }
      biasesMems zip biasesDRAM foreach {
        case (sram, dram) =>
          sram load dram(0.to[I32] :: nHiddenUnits.to[I32])
      }

      val xh: SRAM1[lowT] = SRAM[lowT](nFeatures + nHiddenUnits)
      val c: SRAM1[lowT] = SRAM[lowT](nHiddenUnits)
      xh load xhDRAM(0.to[I32] :: (nFeatures + nHiddenUnits).to[I32])
      c load cInitDRAM(0.to[I32] :: nHiddenUnits.to[I32])

      Sequential.Foreach(nTimeSteps.to[I32] by 1.to[I32]) { _ =>
        // TODO: the two-level loop splitting is a CGRA specific optimization.
        // TODO: A CGRA has dedicated SIMD support for the inner most loop.
        // TODO: However, on an FPGA, the unrolling and vectorization are both overlayed...
        Foreach(nHiddenUnits.to[I32] by 1.to[I32] par hu.to[I32]) { ih =>
          def fusedDotProductWithNonLinear(w: SRAM2[lowT],
                                           nonLinFunc: highT => highT,
                                           b: SRAM1[lowT]): highT = {
            val elem: highT = Reduce(Reg[highT])(
              (nHiddenUnits + nFeatures).to[I32] by rv.to[I32] par ru.to[I32]
            ) { iu =>
              Reduce(Reg[highT])(rv.to[I32] by 1.to[I32] par rv.to[I32]) { iv =>
                val iuv = iu + iv
                (w(ih, iuv) * xh(iuv)).to[highT]
              } {
                _ + _
              }
            } {
              _ + _
            }.value + b(ih).to[highT]
            val re: highT = nonLinFunc(elem)
            re
          }

          val ijfoRegs: scala.Array[Reg[highT]] = scala.Array.tabulate(nGates) {
            _ =>
              Reg[highT]
          }

          Parallel {
            ijfoRegs(0) := fusedDotProductWithNonLinear(ijfoMems(0),
              activationI,
              biasesMems(0))
            ijfoRegs(1) := fusedDotProductWithNonLinear(ijfoMems(1),
              activationJ,
              biasesMems(1))
            ijfoRegs(2) := fusedDotProductWithNonLinear(ijfoMems(2),
              activationF,
              biasesMems(2))
            ijfoRegs(3) := fusedDotProductWithNonLinear(ijfoMems(3),
              activationO,
              biasesMems(3))
          }

          val i = ijfoRegs(0).value
          val j = ijfoRegs(1).value
          val f = ijfoRegs(2).value
          val o = ijfoRegs(3).value

          Pipe {
            val cNew = i * j + c(ih).to[highT] * f
            c(ih) = cNew.to[lowT]
            xh(ih + nFeatures.to[I32]) = (tanh(cNew) * o).to[lowT]
          }
        }
      }

      cResultDRAM store c(0.to[I32] :: nHiddenUnits.to[I32])
      xhResultDRAM store xh(0.to[I32] :: (nHiddenUnits + nFeatures).to[I32])
    }

    val cResult = getMem(cResultDRAM)
    val xhResult = getMem(xhResultDRAM)
    printArray(cResult, "c = ")
    printArray(xhResult, "[x, h] = ")
  }
}
