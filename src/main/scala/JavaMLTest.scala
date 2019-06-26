import org.jpmml.evaluator._

import scala.collection.JavaConverters._
import _root_.java.io.File

import org.dmg.pmml.FieldName

object JavaMLTest {
  def main(args: Array[String]): Unit = {
    val path = "/Users/tianzhao/Developers/spatial-apps/RNN/pmmls/"
    val modelName = "FIFONew_FFs.pmml"
    val evaluator: Evaluator =
      new LoadingModelEvaluatorBuilder().load(new File(path + modelName))build()
    evaluator.verify()

    val modelFields = List(
      "mem_B0",
      "mem_N0",
      "mem_a0",
      "mem_bitwidth",
      "mem_dim0",
      "mem_hist0muxwidth",
      "mem_hist0rlanes",
      "mem_hist0wlanes",
      "mem_nbufs",
      "mem_p0"
    )

    val data = Array(
      1,
      16,
      1,
      32,
      256,
      16,
      1,
      2,
      1,
      16
    )



    // Java part
    val inputFields: Array[InputField] = evaluator.getInputFields.toArray.map(f => f.asInstanceOf[InputField])
    val fieldNames: Array[FieldName] = inputFields.map(f => f.getName)
    val fieldValues: Array[FieldValue] = (inputFields zip data).map {
      case (f, d) =>
        f.prepare(d)
    }
    val args = (fieldNames zip fieldValues).toMap
    val targetFields = evaluator.evaluate(args.asJava)
    println(targetFields)

//    println(inputFields)
  }
}
