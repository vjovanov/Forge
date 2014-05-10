/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic boolean structure.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait ForgeTrieBitmapOps {
  this: OptiGraphDSL =>
  def importForgeTrieBitmapOps() {
    val FSparseBitSet = grp("FSparseBitSet")
    val SparseBitSet = ephemeralTpe("com.zaxxer.sparsebits.SparseBitSet")
    //static (FSparseBitSet) ("apply", Nil, Nil :: SparseBitSet, effect=mutable) implements codegen($cala, ${new com.zaxxer.SparseBitSet()})
    static (FSparseBitSet) ("apply", Nil, MArray(MInt) :: SparseBitSet) implements codegen($cala, ${
      val bm = new com.zaxxer.sparsebits.SparseBitSet($0.length)
      $0.foreach{e => bm.set(e)}
      bm
    })
    direct (FSparseBitSet) ("lengthSBS", Nil, SparseBitSet :: MInt) implements codegen($cala, ${$0.length()})
    direct (FSparseBitSet) ("containsSBS", Nil, (SparseBitSet,MInt) :: MBoolean) implements codegen($cala, ${$0.get($1)})
    direct (FSparseBitSet) ("andSBS", Nil, (SparseBitSet,SparseBitSet) :: SparseBitSet) implements codegen($cala, ${com.zaxxer.sparsebits.SparseBitSet.and($0,$1)})
    //direct (FSparseBitSet) ("or", Nil, (SparseBitSet,SparseBitSet) :: SparseBitSet) implements codegen($cala, ${org.SparseBitSet.SparseBitSet.or($0,$1)})
    //direct (FSparseBitSet) ("orInPlace", Nil, (SparseBitSet,SparseBitSet) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.or($1)})
    //direct (FSparseBitSet) ("andInRange", Nil, (SparseBitSet,SparseBitSet) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.and($1)})
    direct (FSparseBitSet) ("getCardinalitySBS", Nil, SparseBitSet :: MInt) implements codegen($cala, ${$0.cardinality()})
    //direct (FSparseBitSet) ("roaringToArray", Nil, SparseBitSet :: MArray(MInt)) implements codegen($cala, ${$0.toArray()})
    direct (FSparseBitSet) ("cloneSBS", Nil, SparseBitSet :: SparseBitSet, aliasHint = copies(0)) implements codegen($cala, ${$0.clone()})
    direct (FSparseBitSet) ("nextSetBitSBS", Nil, (SparseBitSet,MInt) :: MInt) implements codegen($cala, ${$0.nextSetBit($1)})

    direct (FSparseBitSet) ("printSBS", Nil, SparseBitSet :: MUnit, effect=simple) implements codegen($cala, ${
      var i = 0
      while(i < $0.length()){
        val index = $0.nextClearBit(i)
        println("Data: " + index)
        i = index+1
      }
    })
    direct (FSparseBitSet) ("foreachSBS", Nil, (SparseBitSet,(MInt ==> MUnit)) :: MUnit, effect=simple) implements codegen($cala, ${
      var i = 0
      while(i < $0.length()){
        val index = $0.nextSetBit(i)
        $b[1](index)
        i = index+1
      }
    })
  }
}