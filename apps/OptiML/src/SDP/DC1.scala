import optiml.compiler._
import optiml.library._
import optiml.shared._

object DC1Compiler extends OptiMLApplicationCompiler with DC1
object DC1Interpreter extends OptiMLApplicationInterpreter with DC1

trait DC1 extends OptiMLApplication {
  def printUsage = {
    println("Usage: DC1 <input matrix file> <y0> <n> <alpha> <kmax>")
    exit(-1)
  }

  def main() = {
    if (args.length < 3) printUsage

    val n = args(2).toInt
    val alpha = args(3).toDouble
    val kmax = args(4).toInt
    val a = readSparseMatrixCOO(args(0), n, n)
    val y0 = readVector(args(1)).t

    println("n: " + n)
    println("alpha: " + alpha)
    println("kmax:  " + kmax)

    tic()

    implicit def diffPDIP(t1: Rep[Tup2[DenseVector[Double],Int]],
                          t2: Rep[Tup2[DenseVector[Double],Int]]) = {
      dist(t1._1, t2._1)
    }

    val soln = untilconverged(pack(y0, unit(0)), maxIter = kmax) { cur =>
      val (y, k) = unpack(cur)

      //val ak = alpha / (k + 1)

      val n2y: Rep[Double] = y *:* y
      val dy = ((1.0 / n2y) * (a * y)) - y

      val p1 = -4.0 * (y *:* (a * dy)) + 4.0 * ((y *:* y) * (y *:* dy))
      val p2 = -2.0 * (dy *:* (a * dy)) + 2.0 * ((y *:* y) * (dy *:* dy)) + 4.0 * ((y *:* dy) * (y *:* dy))
      val p3 = 4.0 * ((y *:* dy) * (dy *:* dy))
      val p4 = (dy *:* dy) * (dy *:* dy)

      // make a few iterations of newton's method
      var ak: Rep[Double] = 0.0
      ak = ak - (p1 + 2.0 * p2 * ak + 3.0 * p3 * ak * ak + 4.0 * p4 * ak * ak * ak) / (2.0 * p2 + 6.0 * p3 * ak + 12.0 * p4 * ak * ak)
      ak = ak - (p1 + 2.0 * p2 * ak + 3.0 * p3 * ak * ak + 4.0 * p4 * ak * ak * ak) / (2.0 * p2 + 6.0 * p3 * ak + 12.0 * p4 * ak * ak)
      ak = ak - (p1 + 2.0 * p2 * ak + 3.0 * p3 * ak * ak + 4.0 * p4 * ak * ak * ak) / (2.0 * p2 + 6.0 * p3 * ak + 12.0 * p4 * ak * ak)

      println(ak)

      val y_next = y + ak * dy

      pack(y_next, k + 1)
    }
    val (y_soln, k_soln) = unpack(soln)

    toc(soln)
    
    val err = (a * y_soln) - ((y_soln *:* y_soln) * y_soln)
    println("err: " + sqrt(err *:* err))
  }
}
