import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object ICBUndirectedTriangleCountingCompiler extends OptiGraphApplicationCompiler with ICBUndirectedTriangleCounting

// This object lets us run the Scala library version of the code
object ICBUndirectedTriangleCountingInterpreter extends OptiGraphApplicationInterpreter with ICBUndirectedTriangleCounting

trait ICBUndirectedTriangleCounting extends OptiGraphApplication {
  def main() = {
    println("ICBUndirectedTriangleCounting")

    if (args.length < 1) printUsage

    
    val underForHash = 0//64
    val bitSetMultiplier = 0//32

    tic("input loading")
    val adjList = loadUndirectedAdjList(args(0))
    
    toc("input loading",adjList)
    tic("creating graph",adjList)
    
    val g = createICBUndirectedGraphFromAdjList(adjList,underForHash,bitSetMultiplier)

    toc("creating graph",g)

    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    println("Number of Edges: " + g.numEdges)

    println("performing Traingle Counting: " + g.numNodes)
    tic("Triangle Counting",g)
    
    val t = g.countTriangles()

    toc("Triangle Counting",t)
    println("Number of triangles " + t)
  }
  def printUsage = {
    println("Usage: ICBUndirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
