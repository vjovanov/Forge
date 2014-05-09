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
  	static (FRoaringBitmap) ("apply", Nil, MArray(MInt) :: RoaringBitmap) implements codegen($cala, ${org.roaringbitmap.RoaringBitmap.bitmapOf($0:_*)})
  	direct (FRoaringBitmap) ("contains", Nil, (RoaringBitmap,MInt) :: MBoolean) implements codegen($cala, ${$0.contains($1)})
    direct (FRoaringBitmap) ("and", Nil, (RoaringBitmap,RoaringBitmap) :: RoaringBitmap) implements codegen($cala, ${org.roaringbitmap.RoaringBitmap.and($0,$1)})
    direct (FRoaringBitmap) ("or", Nil, (RoaringBitmap,RoaringBitmap) :: RoaringBitmap) implements codegen($cala, ${org.roaringbitmap.RoaringBitmap.or($0,$1)})
    direct (FRoaringBitmap) ("getCardinality", Nil, RoaringBitmap :: MInt) implements codegen($cala, ${$0.getCardinality()})
    direct (FRoaringBitmap) ("roaringToArray", Nil, RoaringBitmap :: MArray(MInt)) implements codegen($cala, ${$0.toArray()})
	  direct (FRoaringBitmap) ("print", Nil, RoaringBitmap :: MUnit, effect=simple) implements codegen($cala, ${
	  	val itr = $0.iterator()
	  	while(itr.hasNext()){
	  		println("Data: " + itr.next())
	  	}
	  })

	}
}