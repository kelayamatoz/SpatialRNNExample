package FloatTests

import spatial.dsl._

@spatial object HelloFix extends SpatialApp {
  def main(args: Array[String]): Void = {
    type T = FixPt[TRUE, _16, _16]
    val floatOut = ArgOut[T]
    //var a : Float = 0.0
    Accel {
      val inVal = 2.44
      val floatInVal = inVal.to[T]

      //val flut = LUT[Float](1.toInt)(floatInVal)
      //flut = floatInVal(0)

      val f = Reg[T] //ok
      f := floatInVal    //ok

      floatOut := f.value
      //floatOut = flut(0)
    }

    println("floatOut = " + getArg(floatOut))
  }

}
