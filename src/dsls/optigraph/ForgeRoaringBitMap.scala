/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic boolean structure.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait ForgeRoaringBitmapOps {
	this: OptiGraphDSL =>
	def importForgeRoaringBitmapOps() {
		val FRoaringBitmap = grp("FRoaringBitmap")
  	val RoaringBitmap = ephemeralTpe("org.roaringbitmap.RoaringBitmap")
   	static (FRoaringBitmap) ("apply", Nil, Nil :: RoaringBitmap, effect=mutable) implements codegen($cala, ${new org.roaringbitmap.RoaringBitmap()})
  	static (FRoaringBitmap) ("apply", Nil, MArray(MInt) :: RoaringBitmap) implements codegen($cala, ${org.roaringbitmap.RoaringBitmap.bitmapOf($0:_*)})
  	direct (FRoaringBitmap) ("containsRBM", Nil, (RoaringBitmap,MInt) :: MBoolean) implements codegen($cala, ${$0.contains($1)})
    direct (FRoaringBitmap) ("andRBM", Nil, (RoaringBitmap,RoaringBitmap) :: RoaringBitmap) implements codegen($cala, ${org.roaringbitmap.RoaringBitmap.and($0,$1)})
    direct (FRoaringBitmap) ("orRBM", Nil, (RoaringBitmap,RoaringBitmap) :: RoaringBitmap) implements codegen($cala, ${org.roaringbitmap.RoaringBitmap.or($0,$1)})
    direct (FRoaringBitmap) ("orInPlaceRBM", Nil, (RoaringBitmap,RoaringBitmap) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.or($1)})
    direct (FRoaringBitmap) ("andInPlaceRBM", Nil, (RoaringBitmap,RoaringBitmap) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.and($1)})
    direct (FRoaringBitmap) ("getCardinalityRBM", Nil, RoaringBitmap :: MInt) implements codegen($cala, ${$0.getCardinality()})
    direct (FRoaringBitmap) ("roaringToArrayRBM", Nil, RoaringBitmap :: MArray(MInt)) implements codegen($cala, ${$0.toArray()})
	  direct (FRoaringBitmap) ("cloneRBM", Nil, RoaringBitmap :: RoaringBitmap, aliasHint = copies(0)) implements codegen($cala, ${$0.clone()})

	  direct (FRoaringBitmap) ("printRBM", Nil, RoaringBitmap :: MUnit, effect=simple) implements codegen($cala, ${
	  	val itr = $0.iterator()
	  	while(itr.hasNext()){
	  		println("Data: " + itr.next())
	  	}
	  })
	  direct (FRoaringBitmap) ("foreachRBM", Nil, (RoaringBitmap,(MInt ==> MUnit)) :: MUnit, effect=simple) implements codegen($cala, ${
	  	val itr = $0.iterator()
	  	while(itr.hasNext()){
	  		$b[1](itr.next)
	  	}
	  })

	}
}