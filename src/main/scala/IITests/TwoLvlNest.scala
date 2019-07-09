package IITests

import spatial.dsl._

@spatial object TwoLvlNest extends SpatialApp {
  def main(args: Array[String]): Unit = {
    val childLoopBound0 = ArgIn[I32]
    val childLoopBound1 = ArgIn[I32]
    val parentLoopBound = ArgIn[I32]

    val loopBound0: scala.Int = 16
    val loopBound1: scala.Int = 8
    val parentLoopBound0: scala.Int = 4

    setArg(childLoopBound0, 16)
    setArg(childLoopBound1, 8)
    setArg(parentLoopBound, 4)

    val outResult = ArgOut[Int]
    Accel {
      outResult := Reduce(0.to[Int])(parentLoopBound.value by 1.to[I32]) { _ =>
        val childLoopVal0: Int = Reduce(0.to[Int])(childLoopBound0.value by 1.to[I32]) { _ =>
          1.to[Int]
        }{_+_}.value

        val childLoopVal1: Int = Reduce(0.to[Int])(childLoopBound1.value by 1.to[I32]) { _ =>
          1.to[Int]
        }{_+_}.value

        childLoopVal0 + childLoopVal1
      }{_+_}.value
    }

    val result: Int = getArg(outResult)
    println("result = ", result)
    val gold: scala.Int = scala.List.tabulate[scala.Int](parentLoopBound0) { case _ =>
      scala.List.tabulate[scala.Int](loopBound0){ case _ => 1 }.sum +
        scala.List.tabulate[scala.Int](loopBound1){ case _ => 1 }.sum
    }.sum
    println("gold = ", gold)
    println("PASSED = ", gold.to[Int] == result)
  }
}
