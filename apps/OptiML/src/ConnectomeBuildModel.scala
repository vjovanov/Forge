import optiml.compiler._
import optiml.library._
import optiml.shared._

object ConnectomeBuildModelCompiler extends OptiMLApplicationCompiler with ConnectomeBuildModel
object ConnectomeBuildModelInterpreter extends OptiMLApplicationInterpreter with ConnectomeBuildModel

trait ConnectomeBuildModel extends OptiMLApplication {

  def main() = {

    println("reading bv.dat...")
    val bv = readMatrix(args(0), ",")
    val bvecs = bv.getCols(0::3)
    val bvals = bv.getCol(3)
    println("done!")

    println("reading vox...")
    val fetemp = readVector[DenseVector[String]](args(1), { line => line }, "\\|")
    val fe = (0::fetemp.length) { n =>
      val line = fetemp(n)
      val s0 = line(0).toDouble
      val ltx = line(1).fsplit(",")
      val tot_fiber_idx = (0::array_length(ltx)) {i => ltx(i).toInt}
      val lux = line(2).fsplit(",")
      val unique_fiber_idx = (0::array_length(lux)) {i => lux(i).toInt}
      val lvx = line(3).fsplit(";")
      val voxTensors = (0::array_length(lvx)) {i => 
        val u = lvx(i).fsplit(",")
        if(array_length(u) != 9) {
          println("Error: Invalid tensor length.")
          exit(-1)
        }
        val jlu = (0::9) { j => u(j).toDouble }
        (0::3, 0::3) { (u,v) => jlu(u + 3*v) }
        //DenseMatrix.reshape_vector(jlu, 3, 3)
      }
      pack(s0, tot_fiber_idx, unique_fiber_idx, voxTensors)
    }
    println("done!")

    println("reading expected output...")
    val expected_out = readVector[DenseVector[Double]](args(2), { line => 
      (0::line.length) { i => line(i).toDouble } 
    }, ",")
    println("done!")

    // println("reading vox.dat")
    // val fe = readVector[Tup4[Double,DenseVector[Int],DenseVector[Int],DenseVector[DenseMatrix[Double]]]](args(1), {line =>
    //   if(line.length != 4) {
    //     println("input reading error")
    //     exit(-1)
    //   }
    //   println(line)
    //   val s0 = line(0).toDouble
    //   val ltx = line(1).fsplit(",")
    //   val tot_fiber_idx = (0::array_length(ltx)) {i => ltx(i).toInt}
    //   val lux = line(2).fsplit(",")
    //   val unique_fiber_idx = (0::array_length(lux)) {i => lux(i).toInt}
    //   val lvx = line(3).fsplit(";")
    //   val voxTensors = (0::array_length(lvx)) {i => 
    //     val u = lvx(i).fsplit(",")
    //     if(array_length(u) != 9) {
    //       println("Error: Invalid tensor length.")
    //       exit(-1)
    //     }
    //     val jlu = (0::9) { j => u(j).toDouble }
    //     //(0::3, 0::3) { (u,v) => jlu(u + 3*v) }
    //     DenseMatrix.reshape_vector(jlu, 3, 3)
    //   }
    //   pack(s0, tot_fiber_idx, unique_fiber_idx, voxTensors)
    // }, "\\|")
    // println("done!")

    val vox_sparse_pSig = (0::fe.length) { vv =>
      val (s0, tot_fiber_idx, unique_fiber_idx, voxTensors) = unpack(fe(vv))
      val tot_fibers_num = tot_fiber_idx.length
      val unique_fibers_num = unique_fiber_idx.length
      val nTensors = voxTensors.length

      val adc = (0::bv.numRows, 0::nTensors) { (i, j) =>
        val q = voxTensors(j)
        val bvj = bvecs.getRow(i).t
        bvj *:* (q * bvj)
      }

      val voxelPSignalI = (0::bv.numRows, 0::nTensors) { (i, j) =>
        s0 * exp(-bvals(i) * adc(i, j))
      }

      val voxelPSignal = if(tot_fibers_num != unique_fibers_num) {
        val combineM = (0::tot_fibers_num, 0::unique_fibers_num) { (i, j) =>
          if (tot_fiber_idx(i) == unique_fiber_idx(j)) 1.0 else 0.0
        }
        voxelPSignalI * combineM
      }
      else {
        voxelPSignalI
      }

      val meanVoxelPSignal = (0::unique_fibers_num) { i => voxelPSignal.getCol(i).mean }
      val demeaned_pSig = (0::bv.numRows, 0::unique_fibers_num) { (i, j) =>
        voxelPSignal(i, j) - meanVoxelPSignal(j)
      }

      //DenseVector.reshape_matrix(demeaned_pSig)
      //demeaned_pSig
      DenseVector.flatten[Double]((0::bv.numRows) { i => ((0::unique_fibers_num) { j => demeaned_pSig(i, j) }) })
    }

    (0::fe.length) { vv =>
      val r = vox_sparse_pSig(vv)
      val x = expected_out(vv)
      if(r.length != x.length) {
        print("error. lengths do not match. ")
        print(r.length)
        print(" vs ")
        println(x.length)
      }
      else {
        val e = r - x
        val erx = sqrt(e *:* e)
        println(erx)
      }
    }

  }

}
