package BugReportTest

import spatial.dsl._

@spatial object DynamicSRAMBugTest extends SpatialApp {
  //  Reported error:
//[bug] An exception was encountered while compiling DynamicSRAMBugTest:
//[bug]   Cannot convert symbol x29 to a constant Int
//[bug]   java.lang.Exception: Cannot convert symbol x29 to a constant Int
//[bug]   spatial.metadata.types$ParamHelpers.toInt(types.scala:41)
//[bug]   spatial.traversal.banking.ExhaustiveBanking.$anonfun$bankAccesses$51(ExhaustiveBanking.scala:209)
//[bug]   spatial.traversal.banking.ExhaustiveBanking.$anonfun$bankAccesses$51$adapted(ExhaustiveBanking.scala:209)
//[bug]   scala.collection.TraversableLike.$anonfun$map$1(TraversableLike.scala:233)
//[bug]   scala.collection.immutable.List.foreach(List.scala:388)
//[bug]   scala.collection.TraversableLike.map(TraversableLike.scala:233)
//[bug]   scala.collection.TraversableLike.map$(TraversableLike.scala:226)
//[bug]   scala.collection.immutable.List.map(List.scala:294)
//[bug]   spatial.traversal.banking.ExhaustiveBanking.$anonfun$bankAccesses$48(ExhaustiveBanking.scala:209)
//[bug]   scala.collection.mutable.HashMap.getOrElseUpdate(HashMap.scala:82)
//[bug]   spatial.traversal.banking.ExhaustiveBanking.$anonfun$bankAccesses$33(ExhaustiveBanking.scala:206)
//[bug]   scala.collection.TraversableLike.$anonfun$map$1(TraversableLike.scala:233)
//[bug]   scala.collection.immutable.List.foreach(List.scala:388)
//[bug]   scala.collection.TraversableLike.map(TraversableLike.scala:233)
//[bug]   scala.collection.TraversableLike.map$(TraversableLike.scala:226)
//[bug]   scala.collection.immutable.List.map(List.scala:294)
//[bug]   spatial.traversal.banking.ExhaustiveBanking.$anonfun$bankAccesses$30(ExhaustiveBanking.scala:192)
//[bug]   scala.collection.TraversableLike.$anonfun$flatMap$1(TraversableLike.scala:240)
//[bug]   scala.collection.immutable.List.foreach(List.scala:388)
//[bug]   scala.collection.TraversableLike.flatMap(TraversableLike.scala:240)
//[bug]  .. [see /Users/tianzhao/Developers/spatial-apps/RNN/logs/DynamicSRAMBugTest/DynamicSRAMBugTest_exception.log]
//[bug] This is due to a compiler bug. A log file has been created at:
//[bug]   /Users/tianzhao/Developers/spatial-apps/RNN/logs/DynamicSRAMBugTest//DynamicSRAMBugTest_exception.log
//[failed] Total time: 1.5770 seconds

  def main(args: Array[String]): Unit = {
    val in = ArgIn[I32]
    val inVal = 32.to[I32]
    val out = ArgOut[I32]

    setArg(in, inVal)
    Accel {
      val size = in.value
      val testSRAM: SRAM1[I32] = SRAM[I32](size)
      Foreach (size by 1.to[I32]) { i =>
        testSRAM(i) = in.value
      }

      out := testSRAM(3.to[I32]) + testSRAM(4.to[I32])
    }

    println("getArg = " + getArg(out))
  }
}
