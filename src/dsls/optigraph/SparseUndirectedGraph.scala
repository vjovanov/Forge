/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all SparseUndirectedGraph operations.  Glues 
togther all structures and declares SparseUndirectedGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait SparseUndirectedGraphOps{
  this: OptiGraphDSL =>

  def importSparseUndirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val SparseBitSet = ephemeralTpe("com.zaxxer.sparsebits.SparseBitSet")

    //Actual SparseUndirectedGraph declaration
    val SparseUndirectedGraph = tpe("SparseUndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")

    data(SparseUndirectedGraph,("_numNodes",MInt),("_numEdges",MLong),("_externalIDs",MArray(MInt)),("_neighborhoods",MArray(SparseBitSet))) 
    static(SparseUndirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("numEdges",MLong),("exID",MArray(MInt)),("neighborhoods",MArray(SparseBitSet))), SparseUndirectedGraph))) implements allocates(SparseUndirectedGraph,${$numNodes},${$numEdges},${$exID},${$neighborhoods})

    val SparseUndirectedGraphOps = withTpe(SparseUndirectedGraph)     
    SparseUndirectedGraphOps{

      infix("countBitmapTriangles")(Nil :: MLong) implements composite ${
        val neighborhoods = sparse_neighborhoods($self)
        NodeIdView($self.numNodes).mapreduce[Long]({n => 
          val nbrs = neighborhoods(n)
          var count = 0l
          val bs = cloneSBS(nbrs)
          var i = 0
          while(i < lengthSBS(nbrs)){
            val cur = nextSetBitSBS(nbrs,i)
            count += getCardinalitySBS(andSBS(neighborhoods(cur),nbrs)).toLong
            i = cur + 1
          }
          count
          //getCardinality(bs).toLong
        },{(a,b) => a+b},{e=>true})
      }

      infix ("numEdges")(Nil :: MLong) implements getter(0,"_numEdges")      //SparseUndirectedGraph directed or not?
      infix ("isDirected") (Nil :: MBoolean) implements single ${false}

      //get out neighbors
      infix ("outNbrs") (Node :: SparseBitSet) implements single ${sparse_get_nbrs($self,$1)} 
      infix ("inNbrs") (Node :: SparseBitSet) implements single ${sparse_get_nbrs($self,$1)}
      infix ("neighbors") (MInt :: SparseBitSet) implements single ${sparse_get_nbrs($self,Node($1))}
      infix ("neighbors") (Node :: SparseBitSet) implements single ${sparse_get_nbrs($self,$1)}
      compiler ("sparse_get_nbrs") (Node :: SparseBitSet) implements single ${
        sparse_neighborhoods_apply($self,$1.id)
      }

      compiler ("sparse_neighborhoods") (Nil :: MArray(SparseBitSet)) implements getter(0, "_neighborhoods")
      compiler("sparse_neighborhoods_apply")(MInt :: SparseBitSet) implements single ${array_apply(sparse_neighborhoods($self),$1)}
    }
    addGraphCommonOps(SparseUndirectedGraph) 
  } 
}
