import optiml.compiler._
import optiml.library._
import optiml.shared._

object NetflixCompiler extends OptiMLApplicationCompiler with Netflix
object NetflixInterpreter extends OptiMLApplicationInterpreter with Netflix

trait Netflix extends OptiMLApplication { 
  def print_usage = {
    println("Usage: Netflix <train set> <m> <n>")
    exit(-1)
  }

  def main() = {
    val cy = readMatrix[Int](args(0), line => line.toInt, " ")
    //val c = readMatrix(args(0))
    val m = args(1).toInt
    val n = args(2).toInt
    val r = args(3).toInt
    
    val mu = args(4).toDouble
    val sigma = args(5).toDouble

    val alpha = args(6).toDouble

    val cyr = SparseMatrix[Double](n, m, 
      (0::cy.numRows) { k => cy(k, 2).toDouble }, 
      (0::cy.numRows) { k => cy(k, 0) % m }, 
      (0::cy.numRows) { k => cy(k, 1) % n }).finish

    val cyc = SparseMatrix[Double](m, n, 
      (0::cy.numRows) { k => cy(k, 2).toDouble }, 
      (0::cy.numRows) { k => cy(k, 1) % n }, 
      (0::cy.numRows) { k => cy(k, 0) % m }).finish

    println(m)
    println(n)
    println(r)

    println(cyc.numCols)
    println(cyc.numRows)

    println(cyr.numCols)
    println(cyr.numRows)

    // initialize the matrix
    val v0 = (0::(m+n), 0::r) { (i, j) => 
      if(i == j) 1.0 else 0.0
    }

    println(normf(v0))

    println(v0.numRows)
    println(v0.numCols)

    println("test 1")

    val mdvi = (0::n) { i =>
      val ci = cyr.getRow(i).toSparse
      val cii = ci.indices
      val ciy = ci.nz

      val vi = v0.getRow(i).toDense

      if(cii.length == 0) {
        DenseVector.zeros(r)
      }
      else {
        sum(0, cii.length) { k =>
          val vj = v0.getRow(cii(k) + n).toDense
          val y: Rep[Double] = ciy(k)
          val xmy: Rep[Double] = (vi *:* vj) - y
          xmy * vj
        }
      }
    }

    println("test 2")

    val mdvj = (0::m) { i =>
      val ci = cyc.getRow(i).toSparse
      val cii = ci.indices
      val ciy = ci.nz

      val vi = v0.getRow(i + n).toDense

      if(cii.length == 0) {
        DenseVector.zeros(r)
      }
      else {
        sum(0, cii.length) { k =>
          val vj = v0.getRow(cii(k)).toDense
          val y: Rep[Double] = ciy(k)
          val xmy: Rep[Double] = (vi *:* vj) - y
          xmy * vj
        }
      }
    }

    val mdv2 = (0::(m+n), 0::r) { (i, j) =>
      if (i < n) {
        mdvi.apply(i).apply(j)
      }
      else {
        mdvj.apply(i - n).apply(j)
      }
    }

    println(mdv2.numRows)
    println(mdv2.numCols)

    println("mdv2")
    println(normf(mdv2))
  }

  def normf(x: Rep[DenseMatrix[Double]]) = {
    sqrt(sum(0, x.numRows){ i => sum(0, x.numCols) { j =>  
      val y = x(i,j)
      y * y
    }})
  }
}
