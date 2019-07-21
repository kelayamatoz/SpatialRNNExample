import spatial.dsl._

object MultiIterReduceTree extends SpatialApp {
  def main(args: Array[String]): Unit = {
    type T = Int
    val reduceLen: scala.Int = 256
    val reduceTreeWidth: scala.Int = 64
    val vecA: Array[T] = Array.tabulate(I32(reduceLen)) { i => i.to[T] }
    val vecB: Array[T] = Array.tabulate(I32(reduceLen)) { i => i.to[T] }

    val vecADRAM: DRAM1[T] = DRAM[T](reduceLen)
    val vecBDRAM: DRAM1[T] = DRAM[T](reduceLen)

    setMem(vecADRAM, vecA)
    setMem(vecBDRAM, vecB)

    val resultOut: Reg[T] = ArgOut[T]
    Accel {
      val accumReg: Reg[T] = Reg[T](0.to[T])
      val vecAMem: SRAM1[T] = SRAM(reduceLen)
      val vecBMem: SRAM1[T] = SRAM(reduceLen)
      vecAMem load vecADRAM
      vecBMem load vecBDRAM
      Foreach (reduceLen by reduceTreeWidth) { iTile =>
        def reduceTree(treeWidth: scala.Int, iTile: I32): T = {
          scala.List.tabulate(reduceTreeWidth) { ii =>
              val re: T = vecAMem(iTile + I32(ii)) * vecBMem(iTile + I32(ii))
              re
          }.sumTree
        }
        accumReg := mux(
          iTile == 0.to[I32],
          0.to[I32],
          accumReg + reduceTree(
            reduceTreeWidth,
            iTile
          )
        )
      }

      resultOut := accumReg
    }

    val gold: T = scala.List.tabulate[T](reduceLen) { i =>
      vecA(I32(i)) * vecB(I32(i))
    }.reduce((a, b) => a + b)
    val result: T = getArg(resultOut)
    println(r"result = $result")
    println(r"gold = $gold")
    assert(result.eql(gold))
  }
}
