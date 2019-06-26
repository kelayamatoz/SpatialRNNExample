package FloatTests

import spatial.dsl._

@spatial object HelloFloat extends SpatialApp {
  def main(args: Array[String]): Void = {
    //type fixT = FixPt[TRUE, _16, _16]
    val floatOut = ArgOut[Float]
    //var a : Float = 0.0
    Accel {
      val inVal = 2.44
      val floatInVal = inVal.to[Float]
      
      //val flut = LUT[Float](1.toInt)(floatInVal)
      //flut = floatInVal(0)
      
      val f = Reg[Float] //ok
      f := floatInVal    //ok
      
      floatOut := f.value
     //floatOut = flut(0)
    }

    println("floatOut = " + getArg(floatOut))
  }
}
