import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object RoaringUndirectedTriangleCountingCompiler extends OptiGraphApplicationCompiler with RoaringUndirectedTriangleCounting

// This object lets us run the Scala library version of the code
object RoaringUndirectedTriangleCountingInterpreter extends OptiGraphApplicationInterpreter with RoaringUndirectedTriangleCounting

trait RoaringUndirectedTriangleCounting extends OptiGraphApplication {
  def main() = {
    println("RoaringUndirectedTriangleCounting")

    if (args.length < 1) printUsage

    //Works for both directed and undirected, performance 
    val underForHash = 4//args(1).toInt
    //println("Under for hash: " + underForHash)
    val bitSetMultiplier = 32

    tic("input loading")
    val edgeList = loadUndirectedEdgeList(args(0))
    toc("input loading",edgeList)
    tic("creating graph",edgeList)
    val g = sparseUndirectedGraphFromEdgeList(edgeList)
    toc("creating graph",g)
    
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    println("Number of Edges: " + g.numEdges)

    println("performing Traingle Counting1: " + g.numNodes)
    tic("Triangle Counting BitSet",g)
    val t1 = g.countBitmapTriangles()
    toc("Triangle Counting BitSet",t1)
    println("Number of triangles " + t1)
  }
  def printUsage = {
    println("Usage: RoaringUndirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
