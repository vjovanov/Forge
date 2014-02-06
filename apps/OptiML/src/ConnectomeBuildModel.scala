import optiml.compiler._
import optiml.library._
import optiml.shared._

object ConnectomeBuildModelCompiler extends OptiMLApplicationCompiler with ConnectomeBuildModel
object ConnectomeBuildModelInterpreter extends OptiMLApplicationInterpreter with ConnectomeBuildModel

trait ConnectomeBuildModel extends OptiMLApplication {

  def main() = {

    val bv = readMatrix(args(0), ",")
    val fe = readVector[Tup4[Double,DenseVector[Int],DenseVector[Int],DenseMatrix[Double]]](args(1), {line =>
      val s0 = line(0).toDouble
      val ltx = line(1).fsplit(",")
      val tot_fiber_idx = (0::array_length(ltx)) {i => ltx(i).toInt}
      val lux = line(2).fsplit(",")
      val unique_fiber_idx = (0::array_length(lux)) {i => lux(i).toInt}
      val lvx = line(3).fsplit(";")
      val vvx = (0::array_length(lvx)) {i => lvx(i).fsplit(",")}
      val vvv = vvx map {u => (0::array_length(u)) { j => u(j).toDouble }}
      val voxTensors = DenseMatrix(vvv)
      pack(s0, tot_fiber_idx, unique_fiber_idx, voxTensors)
      }, "\\|")

    println(bv)
    println(fe)

  }

}
