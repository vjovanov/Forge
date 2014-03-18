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
      if(line.length != 4) {
        println(line(0))
        println(line(1))
        println(line(2))
        println(line(3))
      }
      val line = fetemp(n)
      val s0 = line(0).toDouble
      val ltx = ftsplit(line(1), ",") //line(1).fsplit(",")
      val tot_fiber_idx = (0::ltx.length) {i => ltx(i).toInt}
      val lux = ftsplit(line(2), ",") //line(2).fsplit(",")
      val unique_fiber_idx = (0::lux.length) {i => lux(i).toInt}
      val lvx = ftsplit(line(3), ";") //line(3).fsplit(";")
      val voxTensors = (0::lvx.length) {i => 
        val u = ftsplit(lvx(i), ",") //.fsplit(",")
        if(u.length != 9) {
          println("Error: Invalid tensor length.")
          println(u.length)
          println(lvx(i))
          println(u)
          exit(-1)
        }
        val jlu = (0::9) { j => u(j).toDouble }
        (0::3, 0::3) { (u,v) => jlu(u + 3*v) }
        //DenseMatrix.reshape_vector(jlu, 3, 3)
      }
      pack(s0, tot_fiber_idx, unique_fiber_idx, voxTensors)
    }
    println("done!")

    // println("reading expected output...")
    // val expected_out = readVector[DenseVector[Double]](args(2), { line => 
    //   (0::line.length) { i => line(i).toDouble } 
    // }, ",")
    // println("done!")

    // val expected_mrows = readVector[Int](args(2), { line => line(0).toInt })
    // val expected_mcols = readVector[Int](args(3), { line => line(0).toInt })
    // val expected_msignal = readVector[Double](args(4), { line => line(0).toDouble })

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

    val M_rcs = (0::fe.length) { vv =>
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

      val vox_sparse_pSig = DenseVector.flatten[Double]((0::bv.numRows) { i => ((0::unique_fibers_num) { j => demeaned_pSig(i, j) }) })
    
      val sparse_rows = (0::(unique_fiber_idx.length * bvals.length)) { i =>
        (i % bvals.length) + (vv * bvals.length) + 1
      }

      val sparse_cols = (0::(unique_fiber_idx.length * bvals.length)) { i =>
        unique_fiber_idx(i % unique_fibers_num)
      }

      pack(sparse_rows, sparse_cols, vox_sparse_pSig)
    }

    val m_rows = DenseVector.flatten[Int](M_rcs map { _._1 })
    val m_cols = DenseVector.flatten[Int](M_rcs map { _._2 })
    val m_signal = DenseVector.flatten[Double](M_rcs map { _._3 })

    val m_output = (0::(m_rows.length)) { i =>
      m_rows(i).makeStr + " " + m_cols(i).makeStr + " " + m_signal(i).makeStr 
    }

    writeVector(m_output, args(2))

    // (0::fe.length) { vv =>
    //   val r = vox_sparse_pSig(vv)
    //   val x = expected_out(vv)
    //   if(r.length != x.length) {
    //     print("error. lengths do not match. ")
    //     print(r.length)
    //     print(" vs ")
    //     println(x.length)
    //   }
    //   else {
    //     val e = r - x
    //     val erx = sqrt(e *:* e)
    //     println(erx)
    //   }
    //}

    // if((m_rows.length != m_cols.length)
    //   ||(m_rows.length != m_signal.length)
    //   ||(m_rows.length != expected_mrows.length)
    //   ||(m_rows.length != expected_mcols.length)
    //   ||(m_rows.length != expected_msignal.length)) {

    //   println("error. output length mismatch.")
    //   println(m_rows.length)
    //   println(m_cols.length)
    //   println(m_signal.length)
    //   println(expected_mrows.length)
    //   println(expected_mcols.length)
    //   println(expected_msignal.length)
    //   exit(-1)
    // }

    // val edfx = (0::m_signal.length) { k =>
    //   if(m_rows(k) != expected_mrows(k)) {
    //     print("error. row indices do not match: ")
    //     print(m_rows(k))
    //     print(" vs ")
    //     println(expected_mrows(k))
    //   }
    //   if(m_cols(k) != expected_mcols(k)) {
    //     print("error. col indices do not match: ")
    //     print(m_cols(k))
    //     print(" vs ")
    //     println(expected_mcols(k))
    //   }
    //   val e = m_signal(k) - expected_msignal(k)
    //   val erx = sqrt(e * e)
    //   erx
    // }

    // println("")
    // print("max error: ")
    // println(edfx.max)

  }

  def ftsplit(x: Rep[String], s: Rep[String]): Rep[DenseVector[String]] = {
    val xs = x.fsplit(s)
    val vs = (0::array_length(xs)) { i => xs(i) }
    vs filter { k => (k != "") }
  }
}
