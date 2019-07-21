import spatial.dsl._

@spatial object AffineTest extends SpatialApp {
  override def main(args: _root_.spatial.lang.Tensor1[_root_.spatial.lang.Text]): _root_.spatial.lang.Void = {
    type T = Int
    val nRows: I32 = I32(128)
    val nCols: I32 = I32(128)
    val data = Matrix.tabulate(nRows, nCols)((_, _) => random[T](100.to[T]))
    val dataMem: DRAM2[T] = DRAM[T](nRows, nCols)
    val dataMemOut: DRAM2[T] = DRAM[T](nRows, nCols)
    setMem(dataMem, data)

    Accel {
      val testMem: SRAM2[T] = SRAM[T](nRows, nCols)
      testMem load dataMem(0.to[I32]::nRows, 0.to[I32]::nCols)
      val testMemOut: SRAM2[T] = SRAM[T](nRows, nCols)
      Foreach (0 until nRows by 1.to[I32]) { i =>
        Foreach(0 until nCols by 1.to[I32]) { j  =>
          val x0: T = testMem(i * 2.to[I32], 3.to[I32] * i + j * 2.to[I32] - 1.to[I32])
          val tmp = x0 * 32.to[T]
          testMem(i * 4.to[I32] + 1.to[I32], j + i * 3.to[I32]) = tmp
        }

        Foreach (0 until nCols by 1.to[I32]) { j =>
          testMemOut(i, j) = testMem(i, j)
        }
      }

      dataMemOut(0.to[I32]::nRows, 0.to[I32]::nCols) store testMemOut
    }

    printMatrix(getMatrix(dataMem))
  }
}
