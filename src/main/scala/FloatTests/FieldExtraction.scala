package FloatTests

import spatial.dsl._

@spatial object FieldExtraction extends SpatialApp {
  def main(args: Array[String]): Unit = {
    type F = Float
    type T = FixPt[TRUE, _8, _8]
    type U23 = FixPt[FALSE, _23, _0]

    val a = 2.5
    val argT = ArgIn[T]
    val argF = ArgIn[F]
    val argTIOut = ArgOut[U8]
    val argTFOut = ArgOut[U8]
    val argFEOut = ArgOut[U8]
    val argFMOut = ArgOut[U23]

    setArg(argT, a.to[T])
    setArg(argF, a.to[F])

    println("aF.bits: " + argF.value.asBits)
    println("aT.bits: " + argT.value.asBits)

    Accel {
      val aT: T = argT.value
      val aF: F = argF.value

      // Format: [S, E, M]
      val aFM: U23 = aF
        .bits(
          I32(0) :: I32(aF.fmt.M.v - 2) // 1 bit for sign, 1 bit for excluding the right
        )
        .as[U23]
      val aFE: U8 = aF
        .bits(
          I32(aF.fmt.M.v - 1) :: I32(aF.fmt.E.v + aF.fmt.M.v - 2)
        )
        .as[U8]

      // Format: [S, I, F]
      val aTF: U8 = aT
        .bits(
          I32(0) :: I32(aT.fmt.F.v - 1)
        )
        .as[U8]
      val aTI: U8 = aT
        .bits(
          I32(aT.fmt.F.v) :: I32(aT.fmt.I.v + aT.fmt.F.v - 1)
        )
        .as[U8]

      argTIOut := aTI
      argTFOut := aTF
      argFMOut := aFM
      argFEOut := aFE
    }

    println("argTIOut = " + getArg(argTIOut).asBits)
    println("argTFOut = " + getArg(argTFOut).asBits)
    println("argFMOut = " + getArg(argFMOut).asBits)
    println("argFEOut = " + getArg(argFEOut).asBits)
  }
}
