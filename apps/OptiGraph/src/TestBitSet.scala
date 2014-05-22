import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object TestBitSetCompiler extends OptiGraphApplicationCompiler with TestBitSet

// This object lets us run the Scala library version of the code
object TestBitSetInterpreter extends OptiGraphApplicationInterpreter with TestBitSet

trait TestBitSet extends OptiGraphApplication {
  def main() = {
  	println("Testing BitSet world")

  	val inArray1 = array_fromfunction[Int](1024, e=>e)
    val inArray2 = array_fromfunction[Int](2048, e=>e*4)

    //val bs1 = BitSet(inArray1)
    //val bs2 = BitSet(inArray2)
    val cbs1 = CompressedBitSet(inArray1)
    val cbs2 = CompressedBitSet(inArray2)
    //val out = cbs1.andCardinality(cbs2)
    println("Output: " + cbs1 + cbs2)
/*
  	println("Creating Bit Set")
  	val bs1 = BitSet(inArray.getRawArray)
  	val bs2 = BitSet(inArray2.getRawArray)

  	println("Array 1: should be 0-64 length(128) cardinality(65)")
  	bs1.print

  	println("Array 2: should be all even #'s from 0-128 length(192) cardinality(65)")
  	bs2.print

  	println("AND result length(128) cardinality(33)")
  	(bs1 & bs2).print
  	println("And cardinality: " + bs1.andCardinalityInRange(124,bs2))
*/
/*
  	val bs3 = BitSet(65)
  	var i = 0
  	while(i < bs3.length){
  		bs3.set(i,true)
  		if(i%2 == 0) bs3.set(i,false)
  		i += 1
  	}
  	println("BS3")
  	bs3.print
  	println("and cardinality: " + bs2.andCardinality(bs3))
    */
  }
}
