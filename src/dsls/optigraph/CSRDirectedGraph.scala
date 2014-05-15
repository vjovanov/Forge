/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all CSRDirectedGraph operations.  Glues 
togther all structures and declares CSRDirectedGraph operations visible
to user.  Inherits graph common ups.

Data is stored as follows.  Internal ID #'s map to external ID's
in the hashmap that is stored.  Internal ID's are 0 to # of nodes
so that data can be mapped in an array effeciently.  No restrictions
on external ID"s except they cannot be 0.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait CSRDirectedGraphOps{
  this: OptiGraphDSL =>

  def importCSRDirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    //Actual CSRDirectedGraph declaration
    val CSRDirectedGraph = tpe("CSRDirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(CSRDirectedGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(CSRDirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt))), CSRDirectedGraph))) implements allocates(CSRDirectedGraph,${$count},${$exID},${$outNodes},${outEdges},${$inNodes},${$inEdges})

    val CSRDirectedGraphOps = withTpe(CSRDirectedGraph)     
    CSRDirectedGraphOps{
      infix ("isDirected") (Nil :: MBoolean) implements single ${true}
      //get out neighbors
      infix ("outNbrs") (MInt :: NodeDataView(MInt)) implements single ${$self.outNbrs(Node($1))}
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = out_node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1))
          else array_length(out_edge_raw_data($self))
        NodeDataView[Int](out_edge_raw_data($self),start,end-start)
      }
      //get in neighbors   
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = in_node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) 
            else array_length(in_edge_raw_data($self)) 
        NodeDataView[Int](in_edge_raw_data($self),start,end-start)
      }
      infix ("outDegree") (Node :: MInt) implements single ${
        val end  = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1)) 
          else array_length(out_edge_raw_data($self))
        end - out_node_apply($self,$1.id) 
      }
      infix ("inDegree") (Node :: MInt) implements single ${
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) 
            else array_length(in_edge_raw_data($self))
        end - in_node_apply($self,$1.id)
      }
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sumOverCollection($self.outNbrs(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sumOverCollection($self.inNbrs(n))(data){e => level(e)==(level(n.id)-1)}
      }

      //Out Node Accessors
      compiler ("out_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outNodes")
      compiler("out_node_apply")(MInt :: MInt) implements single ${array_apply(out_node_raw_data($self),$1)}
      compiler ("out_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outEdges")
      compiler("out_edge_apply")(MInt :: MInt) implements single ${array_apply(out_edge_raw_data($self),$1)}

      //In Node Accessors
      compiler ("in_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inNodes")
      compiler("in_node_apply")(MInt :: MInt) implements single ${array_apply(in_node_raw_data($self),$1)}
      compiler ("in_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inEdges")
      compiler("in_edge_apply")(MInt :: MInt) implements single ${array_apply(in_edge_raw_data($self),$1)}
    }
    addGraphCommonOps(CSRDirectedGraph)
  } 
}