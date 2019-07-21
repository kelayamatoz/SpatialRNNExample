import spatial.dsl._

@spatial object GEMMFlattenedTree extends SpatialApp {
  type T = FixPt[TRUE, _16, _16]

  def main(args: Array[String]): Unit = {

    val M: I32 = 4.to[I32]
    val N: I32 = 128.to[I32]
    val K: I32 = 64.to[I32]

    val baseAddr: I32 = 0.to[I32]
    val baseStride: I32 = 1.to[I32]

    val a = (baseAddr :: M, baseAddr :: K) { (i, j) =>
      ((i + j * K) % 8.to[I32]).to[T]
    }
    val b = (baseAddr :: K, baseAddr :: N) { (i, j) =>
      ((i + j * N) % 8.to[I32]).to[T]
    }
    val c_init = (baseAddr :: M, baseAddr :: N) { (_, _) =>
      0.to[T]
    }

    val A = DRAM[T](M, K)
    val B = DRAM[T](K, N)
    val C = DRAM[T](M, N)

    // *** Set mp and ip > 1
    val kpFlatten: scala.Int = 16
    val mp = 1.to[I32]
    val kp = I32(kpFlatten)
    val np = 16.to[I32]

    setMem(A, a)
    setMem(B, b)
    setMem(C, c_init)

    Accel {
      val sramA: SRAM2[T] = SRAM[T](M, K)
      val sramB: SRAM2[T] = SRAM[T](K, N)
      val sramC: SRAM2[T] = SRAM[T](M, N)

      sramA load A(baseAddr :: M, baseAddr :: K)
      sramB load B(baseAddr :: K, baseAddr :: N)
      sramC load C(baseAddr :: M, baseAddr :: N)

      val lastTile = (K / kp - 1.to[I32]) * kp
      Foreach(M by baseStride par mp) { m =>
        // I'm not sure if this part is written correctly.
        Pipe.II(1).Foreach(N by baseStride par np, K by kp) { (n, kTile) =>
          def reduceTreeDp(): T = {
            scala.List
              .tabulate[T](kpFlatten) { ii =>
                val kk: I32 = kTile + ii.to[I32]
                val re = sramA(m, kk) * sramB(kk, n)
                re
              }
              .sumTree
          }

          val f = Reg[T]
          f := reduceTreeDp() + mux(kTile == 0.to[I32], 0.to[T], f.value)

          if (kTile == lastTile) {
            sramC(m, n) = f.value
          }
        }
      }
      C(baseAddr :: M, baseAddr :: N) store sramC
    }
    val result = getMatrix(C)
    val gold = (baseAddr :: M, baseAddr :: N) { (i, j) =>
      Array
        .tabulate(K) { k =>
          a(i, k) * b(k, j)
        }
        .reduce { _ + _ }
    }
    println(r"expected cksum: ${gold.map(a => a).reduce { _ + _ }}")
    println(r"result cksum: ${result.map(a => a).reduce { _ + _ }}")
    printMatrix(gold, "Gold: ")
    printMatrix(result, "Result: ")
    assert(gold == result)
  }
}
