/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic integer array.
Used as a bitmap for BFS (could be optimized further).
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait AtomicLongArrayOps {
  	this: OptiGraphDSL =>
  	def importAtomicLongArrayOps() {
  		val AtomicLongArray = grp("AtomicLongArray")
    	val AArray = ephemeralTpe("java.util.concurrent.atomic.AtomicLongArray")
    	static (AtomicLongArray) ("apply", Nil, MLong :: AArray, effect=mutable) implements codegen($cala, ${new java.util.concurrent.atomic.AtomicLongArray($0.toInt)})
    	direct (AtomicLongArray) ("testAtomic", Nil, (AArray,MLong,MLong) :: MBoolean) implements composite ${get($0,$1)==$2}
    	direct (AtomicLongArray) ("get", Nil, (AArray,MLong) :: MLong) implements codegen($cala, ${$0.get($1.toInt)})
      direct (AtomicLongArray) ("getAndAdd", Nil, (AArray,MLong,MLong) :: MLong, effect = write(0)) implements codegen($cala, ${$0.getAndAdd($1.toInt,$2)})
	    direct (AtomicLongArray) ("testAndSetAtomic", Nil, (AArray,MLong,MLong,MLong) :: MBoolean, effect=write(0)) implements codegen($cala, ${$0.compareAndSet($1.toInt,$2,$3)})
	    direct (AtomicLongArray) ("set", Nil, (AArray,MLong,MLong) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.set($1.toInt,$2)})
  }
}

trait AtomicDoubleArrayOps {
    this: OptiGraphDSL =>
    def importAtomicDoubleArrayOps() {
      val AtomicDoubleArray = grp("AtomicDoubleArray")
      val AArray = ephemeralTpe("com.google.common.util.concurrent.AtomicDoubleArray")
      static (AtomicDoubleArray) ("apply", Nil, MLong :: AArray, effect=mutable) implements codegen($cala, ${new com.google.common.util.concurrent.AtomicDoubleArray($0.toInt)})
      direct (AtomicDoubleArray) ("get", Nil, (AArray,MLong) :: MDouble) implements codegen($cala, ${$0.get($1.toInt)})
      direct (AtomicDoubleArray) ("getAndAdd", Nil, (AArray,MLong,MDouble) :: MDouble, effect = write(0)) implements codegen($cala, ${$0.getAndAdd($1.toInt,$2)})
      direct (AtomicDoubleArray) ("set", Nil, (AArray,MLong,MDouble) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.set($1.toInt,$2)})
  }
}

trait AtomicBooleanOps {
    this: OptiGraphDSL =>
    def importAtomicBooleanOps() {
      val AtomicBoolean = grp("AtomicBoolean")
      val ABool = ephemeralTpe("java.util.concurrent.atomic.AtomicBoolean")
      static (AtomicBoolean) ("apply", Nil, MBoolean :: ABool, effect=mutable) implements codegen($cala, ${new java.util.concurrent.atomic.AtomicBoolean($0)})
      direct (AtomicBoolean) ("get", Nil, ABool :: MBoolean) implements codegen($cala, ${$0.get()})
      direct (AtomicBoolean) ("getAndSet", Nil, (ABool,MBoolean) :: MBoolean, effect=write(0)) implements codegen($cala, ${$0.getAndSet($1)})
      direct (AtomicBoolean) ("set", Nil, (ABool,MBoolean) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.set($1)})
  }
}