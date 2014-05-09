/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all RoaringUndirectedGraph operations.  Glues 
togther all structures and declares RoaringUndirectedGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait RoaringUndirectedGraphOps{
  this: OptiGraphDSL =>

  def importRoaringUndirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val RoaringBitmap = ephemeralTpe("org.roaringbitmap.RoaringBitmap")


    //Actual RoaringUndirectedGraph declaration
    val RoaringUndirectedGraph = tpe("RoaringUndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")

    data(RoaringUndirectedGraph,("_numNodes",MInt),("_numEdges",MLong),("_externalIDs",MArray(MInt)),("_neighborhoods",MArray(RoaringBitmap))) 
    static(RoaringUndirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("numEdges",MLong),("exID",MArray(MInt)),("neighborhoods",MArray(RoaringBitmap))), RoaringUndirectedGraph))) implements allocates(RoaringUndirectedGraph,${$numNodes},${$numEdges},${$exID},${$neighborhoods})

    val RoaringUndirectedGraphOps = withTpe(RoaringUndirectedGraph)     
    RoaringUndirectedGraphOps{

      infix("countBitmapTriangles")(Nil :: MLong) implements composite ${
        val neighborhoods = roaring_neighborhoods($self)
        NodeIdView($self.numNodes).mapreduce[Long]({n => 
          val nbrs = neighborhoods(n)
          var count = 0l
          val bs = clone(nbrs)
          foreach(nbrs,{nbr => 
            //if(nbr > n){
              //andInPlace(bs,$1(nbr))
              count += getCardinality(and(neighborhoods(nbr),nbrs)).toLong
            //}
          })
          count
          //getCardinality(bs).toLong
        },{(a,b) => a+b},{e=>true})
      }

      infix ("numEdges")(Nil :: MLong) implements getter(0,"_numEdges")      //RoaringUndirectedGraph directed or not?
      infix ("isDirected") (Nil :: MBoolean) implements single ${false}

      //get out neighbors
      infix ("outNbrs") (Node :: RoaringBitmap) implements single ${roaring_get_nbrs($self,$1)} 
      infix ("inNbrs") (Node :: RoaringBitmap) implements single ${roaring_get_nbrs($self,$1)}
      infix ("neighbors") (MInt :: RoaringBitmap) implements single ${roaring_get_nbrs($self,Node($1))}
      infix ("neighbors") (Node :: RoaringBitmap) implements single ${roaring_get_nbrs($self,$1)}
      compiler ("roaring_get_nbrs") (Node :: RoaringBitmap) implements single ${
        roaring_neighborhoods_apply($self,$1.id)
      }

      compiler ("roaring_neighborhoods") (Nil :: MArray(RoaringBitmap)) implements getter(0, "_neighborhoods")
      compiler("roaring_neighborhoods_apply")(MInt :: RoaringBitmap) implements single ${array_apply(roaring_neighborhoods($self),$1)}
    }
    addGraphCommonOps(RoaringUndirectedGraph) 
  } 
}
