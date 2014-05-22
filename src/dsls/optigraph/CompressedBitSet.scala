/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic integer array.
Used as a bitmap for BFS (could be optimized further).
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait CompressedBitSetOps {
  this: OptiGraphDSL =>
  def importCompressedBitSetOps() {
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeData = lookupTpe("NodeData")
    val GraphBitSet = lookupTpe("GraphBitSet")
    val HashSet = lookupTpe("HashSet")
    val NodeCollection = lookupTpe("NodeCollection")
    val CompressedBitSet = tpe("CompressedBitSet")
    val T = tpePar("T")
    val R = tpePar("R")

    /*
      Types for Node Collections
      
      0 - Hash Set
      1 - CSR
      2 - Bit Set
    */
    data(CompressedBitSet,("_length",MInt),("_level1",MArray(MInt)),("_level2",MArray(MHashMap(MInt,NodeCollection))))

    static (CompressedBitSet) ("apply", Nil, MArray(MInt) :: CompressedBitSet) implements allocates(CompressedBitSet,${array_length($0)},${cbs_alloc_level_1($0)},${cbs_alloc_from_int_array2($0)})

    val CompressedBitSetOps = withTpe(CompressedBitSet)
    CompressedBitSetOps{
      //infix ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("getLevel1") (Nil :: MArray(MInt)) implements getter(0, "_level1")
      compiler ("getLevel2") (Nil :: MArray(MHashMap(MInt,NodeCollection))) implements getter(0, "_level2")
      //compiler ("getLevel2") (Nil :: MHashMap(MInt,NodeCollection)) implements getter(0, "_level2")
      /*
      compiler ("binarySearch") ((("key",MInt),("set",MArray(MInt))) :: MInt) implements single ${
        var low = 0
        var high = array_length(set)
        val ikey = key
        var found = false
        var middleIndex = 0
        while (low <= high && !found) {
            middleIndex = (low + high) >> 1;
            val middleValue = set(middleIndex)

            if (middleValue < ikey)
                low = middleIndex + 1
            else if (middleValue > ikey)
                high = middleIndex - 1
            else
                found = true
        }
        if(found) middleIndex
        else -1
      }
      infix ("andCardinality") (("bs2",CompressedBitSet) :: MLong) implements single ${
        val bs1_level1 = getLevel1($self)
        val bs2_level1 = getLevel1($self)

        val bs1_level2 = getLevel2(bs2)
        val bs2_level2 = getLevel2(bs2)
        var count = 0l

        var bs1_l1 = 0
        var bs2_l1 = 0
        while(bs1_l1 < array_length(bs1_level1) && bs2_l1 < array_length(bs2_level1)){
          if(bs1_level1(bs1_l1) < bs2_level1(bs2_l1)){
            bs1_l1 += 1
          }
          else if(bs1_level1(bs1_l1) > bs2_level1(bs2_l1)){
            bs2_l1 += 1
          }
          else{
            //made it to second level
            val bs1_level2_keys = fhashmap_keys(bs1_level2(bs1_l1))
            val bs2_level2_keys = fhashmap_keys(bs2_level2(bs2_l1))

            var bs1_l2 = 0
            var bs2_l2 = 0 
            while(bs1_l2 < array_length(bs1_level2_keys) && bs2_l2 < array_length(bs2_level2_keys)){
              if(bs1_level2_keys(bs1_l2) > bs2_level2_keys(bs2_l2)){
                bs1_l2 += 1
              }
              else if(bs1_level2_keys(bs1_l2) > bs2_level2_keys(bs2_l2)){
                bs2_l2 += 1
              }
              else{
                val nc1 = fhashmap_get(bs1_level2(bs1_l1),bs1_level2_keys(bs1_l2))
                val nc2 = fhashmap_get(bs1_level2(bs1_l1),bs1_level2_keys(bs1_l2))
                if(nc1.colType == 1){
                  if(nc2.colType ==1){
                    nc_getNodeDataView(nc1).intersect(nc_getNodeDataView(nc2))
                  }
                  else{
                    intersect(258,nc_getNodeDataView(nc1),nc_getgraphbitset(nc2))
                  }
                }
                else{
                  if(nc2.colType ==1){
                    intersect(258,nc_getgraphbitset(nc1),nc_getNodeDataView(nc2))
                  }
                  else{
                    nc_getgraphbitset(nc1).andCardinality(nc_getgraphbitset(nc2)).toLong
                  }
                }
                bs2_l1 += 1
                bs2_l2 += 1
              }
              bs1_l1 += 1
              bs2_l1 += 1              
            }
          }
        }
        count       
      }
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("print") (Nil :: MUnit, effect=simple) implements single ${
        val level1 = NodeData(getLevel1($self))
        val level2 = NodeData(getLevel2($self))
        val indicies = NodeData.fromFunction(level1.length,e=>e)

        println("level 1 length(2) : " + indicies.length)
        indicies.foreach{ e=>
          val l2keys = NodeData(fhashmap_keys(level2(e)))
          println("level 2 length(2) : " + l2keys.length)
          l2keys.foreach{ a =>
            val nc = fhashmap_get(level2(e),a)
            if(nc.colType == 1){
              nc_getNodeDataView(nc).foreach{ nd =>
                val dat = (level1(e) << 16) | (a << 8) | nd
                println("Data: " + dat)
              }
            }
            else{
              nc_getgraphbitset(nc).foreach{ bs =>
                val dat = (level1(e) << 16) | (a << 8) | bs
                println("Data: " + dat)
        }}}}
      }
      */
    }
    compiler (CompressedBitSet) ("cbs_alloc_level_1", Nil, ("input",MArray(MInt)) :: MArray(MInt)) implements single ${
      val level1 = NodeData(input).groupBy(e => e>>16, e => (e & 0x0000ffff))
      NodeData(fhashmap_keys(level1)).sort.getRawArray
    }
    compiler (CompressedBitSet) ("cbs_alloc_from_int_array2", Nil, ("input",MArray(MInt)) :: MArray(MHashMap(MInt,NodeCollection))) implements single ${
      val level1 = NodeData(input).groupBy(e => e>>16, e => (e & 0x0000ffff))
      val level1keys = NodeData(fhashmap_keys(level1)).sort
      println("level 1 length: " + level1keys.length)
      level1keys.map{i =>
        val hm2 = NodeData(fhashmap_get(level1,i)).groupBy(e => (e & 0x0000ff00)>>8, e => e & 0x000000ff)
        val keys = NodeData(fhashmap_keys(hm2)).sort
        println("level 2 length: " + keys.length)
        val ncs = keys.map{ e =>
          val contents = fhashmap_get(hm2,e)
          if(array_length(array_buffer_result(contents)) > 32)
            NodeCollection(NodeDataView(array_buffer_result(array_buffer_immutable(contents)),0,array_length(array_buffer_result(array_buffer_immutable(contents)))))
            //NodeCollection(GraphBitSet(array_buffer_result(contents)))
          else
            //NodeCollection(GraphBitSet(array_buffer_result(contents)))
            NodeCollection(NodeDataView(array_buffer_result(array_buffer_immutable(contents)),0,array_length(array_buffer_result(array_buffer_immutable(contents)))))
        }
        fhashmap_from_arrays[Int,NodeCollection](keys.getRawArray,ncs.getRawArray)
      }.getRawArray
    }
    /*
    compiler (CompressedBitSet) ("cbs_alloc_from_int_array", Nil, ("input",MArray(MInt)) :: MArray(MHashMap(MInt,NodeCollection))) implements single ${
      val level1 = NodeData(input).groupBy(e => e>>16, e => (e & 0x0000ffff))
      val level1keys = NodeData(fhashmap_keys(level1)).sort
      println("level 1 length: " + level1keys.length)
      level1keys.map{i =>
        val hm2 = NodeData(fhashmap_get(level1,i)).groupBy(e => (e & 0x0000ff00)>>8, e => e & 0x000000ff)
        val keys = NodeData(fhashmap_keys(hm2)).sort
        println("level 2 length: " + keys.length)
        val ncs = keys.map{ e =>
          val contents = NodeData(fhashmap_get(hm2,e))
          if(contents.length > 32){
            NodeCollection(GraphBitSet(contents.getRawArray))
          }
          else{
            NodeCollection(NodeDataView(contents.getRawArray,0,contents.length))
          }
        }
        fhashmap_from_arrays[Int,NodeCollection](keys.getRawArray,ncs.getRawArray)
      }.getRawArray
    }
    */

  }
}