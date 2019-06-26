package FloatTests

import spatial.dsl._

@spatial object FloatConversions extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type TFloat = Float
    type TFix = FixPt[TRUE, _16, _16]

    val floatOut = ArgOut[TFloat]
    val fixOut = ArgOut[TFix]

    Accel {
      val inVal = 2.44
      val bias = 1.56

      val fixInVal = inVal.to[TFix]
      val floatInVal = inVal.to[TFloat]

      // Test 0: fix to float, float FMA
      floatOut := fixInVal.to[TFloat] * floatInVal + bias.to[TFloat]

      // Test 1: float to fix, fix FMA
      fixOut := floatInVal.to[TFix] * fixInVal + bias.to[TFix]
    }

    println("floatOut = " + getArg(floatOut))
    println("fixOut = " + getArg(fixOut))
  }

}
