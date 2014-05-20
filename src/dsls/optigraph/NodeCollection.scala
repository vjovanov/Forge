/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic integer array.
Used as a bitmap for BFS (could be optimized further).
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeCollectionOps {
  this: OptiGraphDSL =>
  def importNodeCollectionOps() {
  val NodeDataView = lookupTpe("NodeDataView")
  val NodeData = lookupTpe("NodeData")
  val GraphBitSet = lookupTpe("GraphBitSet")
  val HashSet = lookupTpe("HashSet")
  val NodeCollection = tpe("NodeCollection")
  val T = tpePar("T")
  val R = tpePar("R")

  /*
    Types for Node Collections
    
    0 - Hash Set
    1 - CSR
    2 - Bit Set
  */
  data(NodeCollection,("_type",MInt),("_dataHS",HashSet(MInt)),("_dataND",NodeData(MInt)),("_dataBS",GraphBitSet))

  static (NodeCollection) ("apply", Nil, MInt :: NodeCollection) implements allocates(NodeCollection,${nc_0},${hs_fake_alloc},${nd_fake_alloc[Int]},${gbs_fake_alloc})
  static (NodeCollection) ("apply", Nil, HashSet(MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_0},${$0},${nd_fake_alloc[Int]},${gbs_fake_alloc})
  static (NodeCollection) ("apply", Nil, NodeData(MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_1},${hs_fake_alloc},${$0},${gbs_fake_alloc})
  static (NodeCollection) ("apply", Nil, GraphBitSet :: NodeCollection) implements allocates(NodeCollection,${nc_2},${hs_fake_alloc},${nd_fake_alloc[Int]},${$0})

  val NodeCollectionOps = withTpe(NodeCollection)
  NodeCollectionOps{
  
    infix ("colType") (Nil :: MInt) implements getter(0, "_type")
    compiler ("nc_getgraphbitset") (Nil :: GraphBitSet) implements getter(0, "_dataBS")
    compiler ("nc_getNodeData") (Nil :: NodeData(MInt)) implements getter(0, "_dataND")
    compiler ("nc_gethashset") (Nil :: HashSet(MInt)) implements getter(0, "_dataHS")
    compiler ("nc_gethashset_keydata") (Nil :: NodeDataView(MInt)) implements single ${  NodeDataView(nc_gethashset($self).toArray,0,nc_gethashset($self).length)    }
  }
  direct (NodeCollection) ("sumOverCollection", R,CurriedMethodSignature(List(("nc",HashSet(MInt)), ("data",MInt==>R) ,("cond",MInt==>MBoolean)),R), TNumeric(R)) implements composite ${
    NodeDataView(nc.toArray,0,nc.length).mapreduce[R](data,{(a,b) => a+b},cond)
  }
  direct (NodeCollection) ("sumOverCollection", R,CurriedMethodSignature(List(("nc",NodeDataView(MInt)), ("data",MInt==>R) ,("cond",MInt==>MBoolean)),R), TNumeric(R)) implements composite ${
    nc.mapreduce[R](data,{(a,b) => a+b},cond)
  }
  direct (NodeCollection) ("sumOverCollection", R,CurriedMethodSignature(List(("nc",GraphBitSet), ("data",MInt==>R) ,("cond",MInt==>MBoolean)),R), TNumeric(R)) implements composite ${
    nc.mapreduce[R](data,{(a,b) => a+b},cond)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,GraphBitSet,GraphBitSet) :: MLong) implements single ${ 
    val small = $0
    $1.andCardinalityInRange(small,$2).toLong 
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,NodeDataView(MInt),GraphBitSet) :: MLong) implements single ${ 
    intersect($0,$2,$1)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,GraphBitSet,NodeDataView(MInt)) :: MLong) implements single ${ 
    //go through NDV probe BS
    val small = $0
    val bs = $1
    val ndv = $2

    var i = 0
    var count = 0l
    var notDone = ((i+1) < ndv.length) || ((i+1) < bs.length)
    var inRange = true
    while(notDone && inRange){
      inRange = ndv(i) < small
      if(bs(ndv(i)) && inRange) count += 1l
      notDone = i < ndv.length
      i += 1
    }
    count
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,NodeDataView(MInt),NodeDataView(MInt)) :: MLong) implements single ${ 
    $1.intersectInRange($2,$0)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,NodeDataView(MInt),HashSet(MInt)) :: MLong) implements single ${ 
    intersect($0,$2,$1)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,HashSet(MInt),NodeDataView(MInt)) :: MLong) implements single ${ 
    val small = $0
    val hs = $1
    val ndv = $2

    var i = 0
    var count = 0l
    var notDone = ((i+1) < ndv.length) || ((i+1) < hs.length)
    var inRange = true
    while(notDone && inRange){
      inRange = ndv(i) < small
      if(hs.contains(ndv(i)) && inRange) count += 1l
      notDone = i < ndv.length
      i += 1
    }
    count
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,HashSet(MInt),HashSet(MInt)) :: MLong) implements single ${ 
    val small = $0
    val hsSmall = if(($1.length-$0) > ($2.length-$0)) $2 else $1
    val hsLarge = if(($1.length-$0) > ($2.length-$0)) $1.toArray else $2.toArray

    var i = 0
    var count = 0l
    var notDone = ((i+1) < array_length(hsLarge)) || ((i+1) < hsSmall.length)
    var inRange = true
    while(notDone && inRange){
      inRange = hsLarge(i) < small
      if(hsSmall.contains(hsLarge(i)) && inRange) count += 1l
      notDone = i < array_length(hsLarge)
      i += 1
    }
    count
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,HashSet(MInt),GraphBitSet) :: MLong) implements single ${ 
    intersect($0,$2,$1)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,GraphBitSet,HashSet(MInt)) :: MLong) implements single ${ 
    val small = $0
    val hs = $2.toArray
    val bs = $1

    var i = 0
    var count = 0l
    var notDone = ((i+1) < array_length(hs)) || ((i+1) < bs.length)
    var inRange = true
    while(notDone && inRange){
      inRange = hs(i) < small
      if(bs(hs(i)) && inRange) count = count + 1l
      notDone = i < array_length(hs)
      i += 1
    }
    count
  }

  compiler (NodeCollection) ("nc_0", Nil, Nil :: MInt) implements single ${ 0 }
  compiler (NodeCollection) ("nc_1", Nil, Nil :: MInt) implements single ${ 1 }
  compiler (NodeCollection) ("nc_2", Nil, Nil :: MInt) implements single ${ 2 }
  }
}