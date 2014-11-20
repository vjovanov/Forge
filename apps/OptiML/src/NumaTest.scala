import optiml.compiler._
import optiml.library._
import optiml.shared._

object NumaTestCompiler extends OptiMLApplicationCompiler with NumaTest
object NumaTestInterpreter extends OptiMLApplicationInterpreter with NumaTest

trait NumaTest extends OptiMLApplication {
  
  def main() = {
    val a = DenseVectorNuma[Double](10, true)
    println(a.apply(0))
//    for (i <- a.indices) {
//      a(i) = i
//    }
//    a.initialSynch()
  }
}
