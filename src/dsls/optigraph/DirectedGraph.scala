/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all DirectedGraph operations.  Glues 
togther all structures and declares DirectedGraph operations visible
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

trait DirectedGraphOps{
  this: OptiGraphDSL =>

  def importDirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NeighborView = lookupTpe("NeighborView")
    val NodeIdView = lookupTpe("NodeIdView")
    //Actual DirectedGraph declaration
    val DirectedGraph = tpe("DirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(DirectedGraph,("_numNodes",MLong),("_externalIDs",MArray(MLong)),("_outNodes",MArray(MLong)),("_outEdges",MArray(MLong)),("_inNodes",MArray(MLong)),("_inEdges",MArray(MLong))) 
    static(DirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MLong),("exID",MArray(MLong)),("outNodes",MArray(MLong)),("outEdges",MArray(MLong)),("inNodes",MArray(MLong)),("inEdges",MArray(MLong))), DirectedGraph))) implements allocates(DirectedGraph,${$numNodes},${$exID}, ${$outNodes}, ${outEdges},${$inNodes},${$inEdges})

    val DirectedGraphOps = withTpe(DirectedGraph)     
    DirectedGraphOps{
      infix ("numEdges")(Nil :: MLong) implements composite ${array_length(in_edge_raw_data($self)) + array_length(out_edge_raw_data($self))}
      infix ("isDirected") (Nil :: MBoolean) implements composite ${true}

      //get out neighbors
      infix ("outNbrs") (MLong :: NeighborView(MLong)) implements composite ${$self.outNbrs(Node($1))}
      infix ("outNbrs") (Node :: NeighborView(MLong)) implements composite ${
        val start = out_node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1))
          else array_length(out_edge_raw_data($self))
        NeighborView[Long](out_edge_raw_data($self),start,end-start)
      }
      //get in neighbors   
      infix ("inNbrs") (Node :: NeighborView(MLong)) implements composite ${
        val start = in_node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) 
            else array_length(in_edge_raw_data($self)) 
        NeighborView[Long](in_edge_raw_data($self),start,end-start)
      }
      infix ("outDegree") (Node :: MLong) implements composite ${
        val end  = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1)) 
          else array_length(out_edge_raw_data($self))
        end - out_node_apply($self,$1.id) 
      }
      infix ("inDegree") (Node :: MLong) implements composite ${
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) 
            else array_length(in_edge_raw_data($self))
        end - in_node_apply($self,$1.id)
      }
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MLong))),("data",MLong==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.outNbrs(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MLong))),("data",MLong==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.inNbrs(n))(data){e => level(e)==(level(n.id)-1)}
      }
      //Input node ids
      infix ("hasEdge") ((MLong,MLong) :: MBoolean) implements composite ${$self.hasEdge(Node($1),Node($2))}
      infix ("hasEdge") ((Node,Node) :: MBoolean) implements composite ${
        val inNbrs = NodeData($self.inNbrs($1).getRawArray).groupByReduce[Long,Long](e => e, e => e, (a,b) => a)
        val outNbrs = NodeData($self.outNbrs($1).getRawArray).groupByReduce[Long,Long](e => e, e => e, (a,b) => a)
        if(fhashmap_contains[Long,Long](inNbrs,$2.id) || fhashmap_contains[Long,Long](outNbrs,$2.id)) true 
        else false
      }
      //Out Node Accessors
      compiler ("out_node_raw_data") (Nil :: MArray(MLong)) implements getter(0, "_outNodes")
      compiler("out_node_apply")(MLong :: MLong) implements composite ${array_apply(out_node_raw_data($self),$1)}
      compiler ("out_edge_raw_data") (Nil :: MArray(MLong)) implements getter(0, "_outEdges")
      compiler("out_edge_apply")(MLong :: MLong) implements composite ${array_apply(out_edge_raw_data($self),$1)}

      //In Node Accessors
      compiler ("in_node_raw_data") (Nil :: MArray(MLong)) implements getter(0, "_inNodes")
      compiler("in_node_apply")(MLong :: MLong) implements composite ${array_apply(in_node_raw_data($self),$1)}
      compiler ("in_edge_raw_data") (Nil :: MArray(MLong)) implements getter(0, "_inEdges")
      compiler("in_edge_apply")(MLong :: MLong) implements composite ${array_apply(in_edge_raw_data($self),$1)}
    }
    addGraphCommonOps(DirectedGraph)
  } 
}
