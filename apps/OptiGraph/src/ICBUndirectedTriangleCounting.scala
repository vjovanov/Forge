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

    if (args.length < 4) printUsage
    
    //val underForHash = 
    //val bitSetMultiplier = 32

    tic("input")
    val adjList = loadUndirectedAdjList(args(0))
    
    toc("input",adjList)
    tic("creation",adjList)
    
    val g = createICBUndirectedGraphFromAdjList(adjList,args(1).toInt,args(2).toInt,args(3))

    toc("creation",g)

    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    println("Number of Edges: " + g.numEdges)

    println("performing Traingle Counting: " + g.numNodes)
    tic("counting",g)
    
    val t = g.countTriangles()

    toc("counting",t)
    println("Number of triangles " + t)
  }
  def printUsage = {
    println("Usage: ICBUndirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
