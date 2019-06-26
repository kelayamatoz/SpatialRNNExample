import spatial.dsl._

@spatial
object RNNMetaProgrammed_loop_perfection_DF extends SpatialApp {
  // This should take 5180 cycles to run...
  def main(args: Array[String]): Unit = {
    // This is describing the LSTM dataflow
    type lowT = FixPt[TRUE, _4, _4]
    type highT = FixPt[TRUE, _16, _16]

    // TODO: how to set these non-linear functions?
    val tanh: highT => highT = x => {
      // 0.0001616 + 0.86635 * x - 0.00042018 * x^2 - 0.13333 * x^3
      val x2: highT = x * x
      val x3: highT = x2 * x
      val x1Term: highT = x * 0.86635.to[highT]
      val x2Term: highT = (-0.00042018).to[highT] * x2
      val x3Term: highT = (-0.13333).to[highT] * x3
      val y: highT = 0.0001616.to[highT] + x1Term + x2Term + x3Term
      y
    }

    val activation: highT => highT = x => {
      // 5-piece activation function
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
      y
    }

    val activationI: highT => highT = activation
    val activationJ: highT => highT = tanh
    val activationF: highT => highT = activation
    val activationO: highT => highT = activation

    val activations = List(activationI, activationJ, activationF, activationO)

    // TODO: what should be the parallelization and vectorization pars?
    val hu = 1
    val ru = 4
    val rv = 16

    // problem-specific params

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

    val hResultDRAM: DRAM1[lowT] = DRAM[lowT](nHiddenUnits)
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

      val c: SRAM1[lowT] = SRAM[lowT](nHiddenUnits)
      val xh: SRAM1[lowT] = SRAM[lowT](nFeatures + nHiddenUnits)
      c load cInitDRAM(0.to[I32] :: nHiddenUnits.to[I32])
      xh load xhDRAM(0.to[I32] :: (nFeatures + nHiddenUnits).to[I32])

      val hNew: SRAM1[lowT] = SRAM[lowT](nHiddenUnits)
      val cNew: SRAM1[lowT] = SRAM[lowT](nHiddenUnits)

      Sequential.Foreach(nTimeSteps by 1.to[I32]) { _ =>
        val lastIterUV: I32 = (((nHiddenUnits + nFeatures) / (ru * rv) - 1) * (ru * rv)).to[I32]
        val lastIterH: I32 = (nHiddenUnits - 1).to[I32]
        Foreach(nHiddenUnits.to[I32] by 1.to[I32] par hu.to[I32]) { ih =>
          Foreach((nHiddenUnits + nFeatures).to[I32] by (ru * rv).to[I32]) { iuvTile =>
            val ijfoRegs = List.tabulate(nGates)(_ => Reg[highT])

            def reduceTreeDP(w: SRAM2[lowT]): highT = {
              List.tabulate(ru * rv) { ii =>
                val iuv = iuvTile + ii.to[I32]
                val re: highT = (w(ih, iuv) * xh(iuv)).to[highT]
                re
              }.sumTree
            }

            def addBiasAndNonLinear(src: highT, biasMem: SRAM1[lowT], nonLinFunc: highT => highT): highT = {
              val elem = src + biasMem(ih).to[highT]
              nonLinFunc(elem)
            }

            val accumRegs = List.tabulate(nGates)(_ => Reg[highT])
            accumRegs.zip(ijfoMems).foreach {
              case (f: Reg[highT], weight: SRAM2[lowT]) =>
              //              if (iuvTile == 0.to[I32])
              //                f.reset()
              //              else
                f := reduceTreeDP(weight) + f.value
            }

            val accums = accumRegs.map(f => f.value)

            if (iuvTile == lastIterUV) {
              val i: highT = addBiasAndNonLinear(accums(0), biasesMems(0), activations(0))
              val j: highT = addBiasAndNonLinear(accums(1), biasesMems(1), activations(1))
              val f: highT = addBiasAndNonLinear(accums(2), biasesMems(2), activations(2))
              val o: highT = addBiasAndNonLinear(accums(3), biasesMems(3), activations(3))

              val cPrime = i * j + c(ih).to[highT] * f
              cNew(ih) = cPrime.to[lowT]
              hNew(ih) = (tanh(cPrime) * o).to[lowT]
            }
          }
        }

        cResultDRAM store cNew(0.to[I32] :: nHiddenUnits.to[I32])
        hResultDRAM store hNew(0.to[I32] :: nHiddenUnits.to[I32])
      }
    }

    val cResult = getMem(cResultDRAM)
    val hResult = getMem(hResultDRAM)
    printArray(cResult, "c = ")
    printArray(hResult, "h = ")
  }
}
