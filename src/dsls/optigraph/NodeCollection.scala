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
  data(NodeCollection,("_type",MInt),("_dataHS",HashSet(MInt)),("_dataNV",NodeDataView(MInt)),("_dataBS",GraphBitSet))

  static (NodeCollection) ("apply", Nil, HashSet(MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_0},${$0},${ndv_fake_alloc},${gbs_fake_alloc})
  static (NodeCollection) ("apply", Nil, NodeDataView(MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_1},${hs_fake_alloc},${$0},${gbs_fake_alloc})
  static (NodeCollection) ("apply", Nil, GraphBitSet :: NodeCollection) implements allocates(NodeCollection,${nc_2},${hs_fake_alloc},${ndv_fake_alloc},${$0})

  val NodeCollectionOps = withTpe(NodeCollection)
  NodeCollectionOps{
  
    infix ("colType") (Nil :: MInt) implements getter(0, "_type")
    compiler ("nc_getgraphbitset") (Nil :: GraphBitSet) implements getter(0, "_dataBS")
    compiler ("nc_getNodeDataView") (Nil :: NodeDataView(MInt)) implements getter(0, "_dataNV")
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
  direct (NodeCollection) ("intersect", Nil, (MInt,GraphBitSet,MInt,GraphBitSet) :: MLong) implements single ${ 
    val small = if($0 < $2) $0 else $2
    $1.andCardinality(small,$3).toLong 
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,NodeDataView(MInt),MInt,GraphBitSet) :: MLong) implements single ${ 
    intersect($2,$3,$0,$1)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,GraphBitSet,MInt,NodeDataView(MInt)) :: MLong) implements single ${ 
    //go through NDV probe BS
    val small = if($0 < $2) $0 else $2
    val bs = $1
    val ndv = $3

    var i = 0
    var count = 0l
    var notDone = i < ndv.length
    var inRange = true
    while(notDone && inRange){
      inRange = ndv(i) < small
      if(bs(ndv(i)) && inRange) count += 1l
      notDone = i < ndv.length
      i += 1
    }
    count
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,NodeDataView(MInt),MInt,NodeDataView(MInt)) :: MLong) implements single ${ 
    $1.intersect($3,$0,$2)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,NodeDataView(MInt),MInt,HashSet(MInt)) :: MLong) implements single ${ 
    intersect($2,$3,$0,$1)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,HashSet(MInt),MInt,NodeDataView(MInt)) :: MLong) implements single ${ 
    val small = if($0 < $2) $0 else $2
    val hs = $1
    val ndv = $3

    var i = 0
    var count = 0l
    var notDone = i < ndv.length
    var inRange = true
    while(notDone && inRange){
      inRange = ndv(i) < small
      if(hs.contains(ndv(i)) && inRange) count += 1l
      notDone = i < ndv.length
      i += 1
    }
    count
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,HashSet(MInt),MInt,HashSet(MInt)) :: MLong) implements single ${ 
    val small = if($0 < $2) $0 else $2
    val hsSmall = if($1.length > $3.length) $3 else $1
    val hsLarge = if($1.length > $3.length) $1.toArray else $3.toArray

    var i = 0
    var count = 0l
    var notDone = i < array_length(hsLarge)
    var inRange = true
    while(notDone && inRange){
      inRange = hsLarge(i) < small
      if(hsSmall.contains(hsLarge(i)) && inRange) count += 1l
      notDone = i < array_length(hsLarge)
      i += 1
    }
    count
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,HashSet(MInt),MInt,GraphBitSet) :: MLong) implements single ${ 
    intersect($2,$3,$0,$1)
  }
  direct (NodeCollection) ("intersect", Nil, (MInt,GraphBitSet,MInt,HashSet(MInt)) :: MLong) implements single ${ 
    val small = if($0 < $2) $0 else $2
    val hs = $3.toArray
    val bs = $1

    var i = 0
    var count = 0l
    var notDone = i < array_length(hs)
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