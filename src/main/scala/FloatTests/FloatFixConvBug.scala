package FloatTests
import spatial.dsl._

@spatial object FloatFixConvBug extends SpatialApp {
  def main(args: Array[String]): Unit = {
    type fixT = FixPt[TRUE, _16, _16]
    val floatOut = ArgOut[Float]

    Accel {
      val inVal = 2.44
      val floatInVal = inVal.to[Float]

      val f = Reg[Float]
      f := floatInVal

//      val a = f.value
      val a = floatInVal
      floatOut = a
    }

    println("floatOut = " + getArg(floatOut))
  }
}
