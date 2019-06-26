package BugReportTest

import spatial.dsl._

@spatial object DynamicParTest extends SpatialApp {
  // Reported error:
//[bug] An exception was encountered while compiling DynamicParTest:
//[bug]   Cannot convert symbol x29 to a constant Int
//[bug]   java.lang.Exception: Cannot convert symbol x29 to a constant Int
//[bug]   spatial.metadata.types$ParamHelpers.toInt(types.scala:41)
//[bug]   spatial.metadata.control.package$IndexHelperOps.$anonfun$ctrParOr1$1(package.scala:852)
//[bug]   spatial.metadata.control.package$IndexHelperOps.$anonfun$ctrParOr1$1$adapted(package.scala:852)
//[bug]   scala.Option.map(Option.scala:146)
//[bug]   spatial.metadata.control.package$IndexHelperOps.ctrParOr1(package.scala:852)
//[bug]   spatial.traversal.AccessExpansion.$anonfun$getUnrolledMatrices$1(AccessExpansion.scala:66)
//[bug]   spatial.traversal.AccessExpansion.$anonfun$getUnrolledMatrices$1$adapted(AccessExpansion.scala:66)
//[bug]   scala.collection.TraversableLike.$anonfun$map$1(TraversableLike.scala:233)
//[bug]   scala.collection.immutable.List.foreach(List.scala:388)
//[bug]   scala.collection.generic.TraversableForwarder.foreach(TraversableForwarder.scala:34)
//[bug]   scala.collection.generic.TraversableForwarder.foreach$(TraversableForwarder.scala:34)
//[bug]   scala.collection.mutable.ListBuffer.foreach(ListBuffer.scala:43)
//[bug]   scala.collection.TraversableLike.map(TraversableLike.scala:233)
//[bug]   scala.collection.TraversableLike.map$(TraversableLike.scala:226)
//[bug]   scala.collection.AbstractTraversable.map(Traversable.scala:104)
//[bug]   spatial.traversal.AccessExpansion.getUnrolledMatrices(AccessExpansion.scala:66)
//[bug]   spatial.traversal.AccessExpansion.getUnrolledMatrices$(AccessExpansion.scala:58)
//[bug]   spatial.traversal.AccessAnalyzer.getUnrolledMatrices(AccessAnalyzer.scala:17)
//[bug]   spatial.traversal.AccessAnalyzer.setAccessPattern(AccessAnalyzer.scala:221)
//[bug]   spatial.traversal.AccessAnalyzer.visit(AccessAnalyzer.scala:311)
//[bug]  .. [see /Users/tianzhao/Developers/spatial-apps/RNN/logs/DynamicParTest/DynamicParTest_exception.log]
//[bug] This is due to a compiler bug. A log file has been created at:
//[bug]   /Users/tianzhao/Developers/spatial-apps/RNN/logs/DynamicParTest//DynamicParTest_exception.log

  def main(args: Array[String]): Unit = {
    val in = ArgIn[I32]
    val inVal = 32.to[I32]
    val out = ArgOut[I32]

    setArg(in, inVal)

    Accel {
      val p = in.value
      val testSRAM: SRAM1[I32] = SRAM[I32](32.to[I32])
      Foreach (32.to[I32] by 1.to[I32] par p) { i =>
        testSRAM(i) = in.value
      }

      out := testSRAM(3.to[I32]) + testSRAM(4.to[I32])
    }

    println("getArg = " + getArg(out))
  }
}
