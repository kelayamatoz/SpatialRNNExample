import spatial.dsl._

@spatial object MatMult extends SpatialApp {

  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {
    val mm = args(0).to[Int]
    val nn = args(1).to[Int]
    val pp = args(2).to[Int]

    val M = ArgIn[Int]; setArg(M,args(0).to[Int])
    val N = ArgIn[Int]; setArg(N,args(1).to[Int])
    val P = ArgIn[Int]; setArg(P,args(2).to[Int])

    val a = (0::M,0::P)[T]{ (i,j) => ((i*P + j)%8).to[T] }
    val b = (0::P,0::N)[T]{ (i,j) => ((i*N + j)%8).to[T] }

    val A = DRAM[T](M, P)
    val B = DRAM[T](P, N)
    val C = DRAM[T](M, N)

    val bm = 16; val bn = 64; val bp = 64

    val pM = 2; val pN = 2; val pm = 2; val pn = 2; val ip = 4;

    setMem(A, a)
    setMem(B, b)

    Accel {
      Foreach(P by bp){k =>
        val tileC = SRAM[T](bm, bn)

        'MAINPIPE.Foreach(M by bm par pM, N by bn par pN){(i,j) =>
          val tileA = SRAM[T](bm, bp)
          val tileB = SRAM[T](bp, bn)
          Parallel {
            tileA load A(i::i+bm, k::k+bp)
            tileB load B(k::k+bp, j::j+bn)
          }
          Foreach(bm by 1 par pm, bn by 1 par pn){ (ii,jj) =>
            val prod = Reduce(Reg[T])(bp by 1 par ip){kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
            val prev = mux(k == 0, 0.to[T], tileC(ii,jj))
            tileC(ii,jj) = prev + prod.value
          }
          C(i::i+bm, j::j+bn) store tileC
        }
      }
    }
    val result = getMatrix(C)
    printMatrix(result, "Result")

    val gold = (0::M, 0::N)[T]{(i,j) =>
      Array.tabulate(P){k => a(i,k) * b(k,j)}.reduce{_+_}
    }
    println(r"Validates? ${gold == result}")
  }

}