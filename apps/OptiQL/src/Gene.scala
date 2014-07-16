import optiql.compiler._
import optiql.library._
import optiql.shared._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.Record


object GeneInterpreter extends OptiQLApplicationInterpreter with GeneApp
object GeneCompiler extends OptiQLApplicationCompiler with GeneApp

trait GeneApp extends OptiQLApplication {

  type Content = Record {
    val header: String
    val geneSeq: String
    val dc3: String
    val dc4: String
  }

  def printUsage() = {
    println("Usage: Gene <input Gene file>")
    exit(-1)
  }

  val g = "AGAT"

  //first characters should match g but with errors ('N') allowed
  def fuzzyStartsWith(s: Rep[String]) = {
    if (s.length < g.length) false
    else {
      var i: Int = 0
      var matched = true
      while (i < g.length) { //unrolled
        if (s.fcharAt(i) != 'N' && s.fcharAt(i) != g(i)) matched = false
        i += 1
      }
      matched
    }
  }

  def main() = {
    if (args.length < 1) printUsage()
    val path = args(0)
    /*val data = Table.fromFile[Content](path, "\t")
    tic(data.size)
    val q = data.Where(r => fuzzyStartsWith(r.geneSeq)).Select(r => r.geneSeq.substring(13))
    toc(q)
    */

    val data = Table.fromFile(path){ line =>
      val fields = array_string_split(line, "\\s+")
      fields(1)
    }
    tic(data.size)
    val q = data.Where(g => fuzzyStartsWith(g)).Select(g => g.substring(13))
    toc(q)
  }

}
