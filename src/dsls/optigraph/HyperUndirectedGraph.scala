/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all Graph operations.  Glues 
togther all structures and declares Graph operations visible
to user.

Common operations for both directed and undirected graphs.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait HyperUndirectedGraphOps{
  this: OptiGraphDSL =>

  def addHyperUndirectedGraphCommonOps(g: Rep[DSLType]) {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val GraphBitSet = lookupTpe("GraphBitSet")
    val HashSet = lookupTpe("HashSet")

    //Actual Graph declaration
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val HyperUndirectedGraph = g
    val HyperUndirectedGraphCommonOps = withTpe(HyperUndirectedGraph)
    HyperUndirectedGraphCommonOps{
      //given an ID return a node
      infix("getNodeFromID")(MInt :: Node) implements composite ${
        val result = NodeIdView($self.numNodes).mapreduce[Int]( i => i, (a,b) => a+b, i => $self.getExternalID(i)==$1)
        if(result >= $self.numNodes() || result < 0) fatal("ERROR. ID: " + $1 + " does not exist in this UndirectedGraph!")
        Node(result)
      }
      infix ("numNodes")(Nil :: MInt) implements getter(0,"_numNodes")
      infix ("foreachNode") ((Node ==> MUnit) :: MUnit, effect = simple) implements composite ${
        NodeData(array_fromfunction($self.numNodes,{n => n})).foreach{ i =>
          $1(Node(i))
        }
      }
      infix("sumOverNodes")  ( (Node ==> R) :: R, TNumeric(R), addTpePars=R) implements composite ${
        NodeIdView($self.numNodes).mapreduce[R]({n => $1(Node(n))},{(a,b) => a+b},{n => true})
      }
      infix("mapNodes")( (Node==>R) :: NodeData(R), addTpePars=R) implements composite ${
        NodeData[R](array_fromfunction($self.numNodes,{n => $1(Node(n))}))
      }
      infix ("getExternalIDs") (Nil :: MArray(MInt)) implements getter(0, "_externalIDs")
      infix ("getExternalID") (MInt :: MInt) implements composite ${array_apply($self.getExternalIDs,$1)}
      
      compiler ("icb_get_csr_nbrs") (MInt :: NodeDataView(MInt)) implements composite ${
        val start = icb_node_apply($self,$1)
        val end = if( ($1+1) < array_length(icb_node_raw_data($self)) ) icb_node_apply($self,($1+1))
          else array_length(icb_edge_raw_data($self))
        NodeDataView[Int](icb_edge_raw_data($self),start,end-start)
      }
      compiler ("icb_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_csrNodes")
      compiler("icb_node_apply")(MInt :: MInt) implements composite ${array_apply(icb_node_raw_data($self),$1)}
      compiler ("icb_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_csrEdges")
      compiler("icb_edge_apply")(MInt :: MInt) implements composite ${array_apply(icb_edge_raw_data($self),$1)}
      compiler ("icb_hash_neighbors") (Nil :: MArray(HashSet(MInt))) implements getter(0, "_hashNeighbors")
      compiler("icb_hash_apply")(MInt :: HashSet(MInt)) implements composite ${array_apply(icb_hash_neighbors($self),$1)}
      compiler ("icb_bs_neighbors") (Nil :: MArray(GraphBitSet)) implements getter(0, "_bsNeighbors")
      compiler("icb_bs_apply")(MInt :: GraphBitSet) implements composite ${array_apply(icb_bs_neighbors($self),$1)}
    }
  } 
}
