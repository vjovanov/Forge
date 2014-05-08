/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all CSRBigUndirectedGraph operations.  Glues 
togther all structures and declares CSRBigUndirectedGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait CSRBigUndirectedGraphOps{
  this: OptiGraphDSL =>

  def importCSRBigUndirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")

    //Actual CSRBigUndirectedGraph declaration
    val CSRBigUndirectedGraph = tpe("CSRBigUndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")

    data(CSRBigUndirectedGraph,("_numNodes",MInt),("_numEdges",MLong),("_externalIDs",MArray(MInt)),("_nodes",MArray(MInt)),("_edges1",MArray(MInt)),("_edges2",MArray(MInt))) 
    static(CSRBigUndirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("numEdges",MLong),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges1",MArray(MInt)),("outEdges2",MArray(MInt))), CSRBigUndirectedGraph))) implements allocates(CSRBigUndirectedGraph,${$numNodes},${$numEdges},${$exID},${$outNodes},${outEdges1},${outEdges2})

    val CSRBigUndirectedGraphOps = withTpe(CSRBigUndirectedGraph)     
    CSRBigUndirectedGraphOps{
      infix ("numEdges")(Nil :: MLong) implements getter(0,"_numEdges")      //CSRBigUndirectedGraph directed or not?
      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      //Perform a sum over the neighbors
      infix ("sumOverNbrs") ( CurriedMethodSignature(List(("n",Node),("data",MInt==>R),("cond",MInt==>MBoolean)),R), TNumeric(R), addTpePars=R) implements composite ${
        sumOverCollection($self.neighbors(n))(data)(cond)
      }
      //Perform a sum over the neighbors
      infix ("sumOverNbrs") ( CurriedMethodSignature(List(("n",MInt),("data",MInt==>R),("cond",MInt==>MBoolean)),R), TNumeric(R), addTpePars=R) implements composite ${
        sumOverCollection($self.neighbors(n))(data)(cond)
      }
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sumOverCollection($self.outNbrs(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        sumOverCollection($self.inNbrs(n))(data){e => (level(e)==(level(n.id)-1))}
      }
      //get out neighbors
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements single ${big_get_nbrs($self,$1)} 
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements single ${big_get_nbrs($self,$1)}
      infix ("neighbors") (MInt :: NodeDataView(MInt)) implements single ${big_get_nbrs($self,Node($1))}
      infix ("neighbors") (Node :: NodeDataView(MInt)) implements single ${big_get_nbrs($self,$1)}
      compiler ("big_get_nbrs") (Node :: NodeDataView(MInt)) implements single ${
        if($1.id < ($self.numNodes/8) ){
          val start = big_node_apply($self,$1.id)
          val end = if( ($1.id+1) != ($self.numNodes/8) ) big_node_apply($self,($1.id+1))
            else array_length(edge_raw_data1($self))
          NodeDataView[Int](edge_raw_data1($self),start,end-start)
        }
        else{
          val start = big_node_apply($self,$1.id)
          val end = if( ($1.id+1) < array_length(big_node_raw_data($self)) ) big_node_apply($self,($1.id+1))
            else array_length(edge_raw_data2($self))
          NodeDataView[Int](edge_raw_data2($self),start,end-start)
        }
      }

      compiler ("big_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("big_node_apply")(MInt :: MInt) implements single ${array_apply(big_node_raw_data($self),$1)}
      compiler ("edge_raw_data1") (Nil :: MArray(MInt)) implements getter(0, "_edges1")
      compiler("edge_apply1")(MInt :: MInt) implements single ${array_apply(edge_raw_data1($self),$1)}
      compiler ("edge_raw_data2") (Nil :: MArray(MInt)) implements getter(0, "_edges2")
      compiler("edge_apply2")(MInt :: MInt) implements single ${array_apply(edge_raw_data2($self),$1)}
    }
    addGraphCommonOps(CSRBigUndirectedGraph) 
  } 
}
