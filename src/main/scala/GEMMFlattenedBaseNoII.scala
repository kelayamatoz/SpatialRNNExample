import spatial.dsl._

@spatial object GEMMFlattenedBaseNoII extends SpatialApp {
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
    val mp = 1.to[I32]
    val kp = 16.to[I32]
    val np = 16.to[I32]

    setMem(A, a)
    setMem(B, b)
    setMem(C, c_init)

    Accel {
      val sramA = SRAM[T](M, K)
      val sramB = SRAM[T](K, N)
      val sramC = SRAM[T](M, N)

      sramA load A(baseAddr :: M, baseAddr :: K)
      sramB load B(baseAddr :: K, baseAddr :: N)
      sramC load C(baseAddr :: M, baseAddr :: N)

      Foreach(
        M by baseStride par mp, N by baseStride par np
      ) { (m, n) =>
        sramC(m, n) = Reduce(0.to[
          T])(K by baseStride par kp) { k =>
          sramA(m, k) * sramB(k, n)
        } { _ + _ }.value
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
    println(r"expected: ${gold.map(a => a).reduce { _ + _ }}")
    println(r"result: ${result.map(a => a).reduce { _ + _ }}")
    printMatrix(gold, "Gold: ")
    printMatrix(result, "Result: ")
    assert(gold == result)
  }
}
