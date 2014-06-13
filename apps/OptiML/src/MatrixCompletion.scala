import optiml.compiler._
import optiml.library._
import optiml.shared._

object MatrixCompletionCompiler extends OptiMLApplicationCompiler with MatrixCompletion
object MatrixCompletionInterpreter extends OptiMLApplicationInterpreter with MatrixCompletion

trait MatrixCompletion extends OptiMLApplication {
  def printUsage = {
    println("Usage: MatrixCompletion <input data file> <nrows> <ncols>")
    exit(-1)
  }

  def main() = {
    if (args.length != 3) printUsage

    val nr = args(2).toInt
    var nc = args(3).toInt

    val mBuilder = SparseMatrix[Double](nr, nc)
    var tmpx = readVector[DenseVector[String]](args(1), { line => 
      mBuilder(line(0).toInt, line(1).toInt) = line(2).toDouble 
    }, " ")
    val a = mBuilder.finish

    println(nr)
    println(nc)
    println(a)
  }
}
