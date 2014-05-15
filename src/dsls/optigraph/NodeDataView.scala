/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Lets us view part of an NodeData array as a 
parallel collection.  This is especially useful when wanting 
to perform operations on neighbors (a subset of edge array).
Here you look at actual data inside of the array.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait NodeDataViewOps {
  this: OptiGraphDSL =>
  def importNodeDataViewOps() {
    val NodeData = lookupTpe("NodeData")
    val T = tpePar("T")
    val R = tpePar("R")
    val NodeDataView = tpe("NodeDataView",T)

    data(NodeDataView, ("_data", MArray(T)), ("_start", MInt), ("_length", MInt))
    static (NodeDataView) ("apply", T, (MArray(T), MInt, MInt) :: NodeDataView(T)) implements allocates(NodeDataView, ${$0}, ${$1}, ${$2})
    val NodeDataViewOps = withTpe(NodeDataView)
    NodeDataViewOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(NodeDataView_data($self), NodeDataView_start($self) + $1) }
      infix ("mapreduce") ( (T ==> R,(R,R) ==> R, T==>MBoolean) :: R, TNumeric(R), addTpePars=(T,R)) implements mapReduce((T,R), 0, ${e => $1(e)}, ${numeric_zero[R]}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}) )
      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${a => $1(a)})
      infix ("start") (Nil :: MInt) implements single ${NodeDataView_start($self)}

      infix ("intersectInRange") ((("nbrsOfNbrs",NodeDataView(T)),("nbrsMax",T)) :: MLong, TNumeric(T)) implements single ${
        val nbrs = $self

        if(nbrs.length == 0 || nbrsOfNbrs.length == 0) 0l
        else if(nbrsMax <= nbrsOfNbrs(0) ||
          nbrsMax <= nbrs(0)){
          0l
        }
        else if(nbrs(0) > nbrsOfNbrs(nbrsOfNbrs.length-1) || 
          nbrsOfNbrs(0) > nbrs(nbrs.length-1)){
          0l
        }
        else if( nbrsOfNbrs.length*64 < nbrs.length || nbrs.length*64 < nbrsOfNbrs.length ){
          ndv_intersect_gallop_in_range($self,nbrsOfNbrs,nbrsMax)
        }
        else{
          ndv_intersect_sets_in_range(nbrs,nbrsOfNbrs,nbrsMax)
        }
      }
      compiler ("ndv_intersect_sets_in_range") ((("nbrsOfNbrs",NodeDataView(T)),("nbrsMax",T)) :: MLong, TNumeric(T)) implements single ${
        val nbrs = $self
        var t = 0l
        var i = 0
        var j = 0
        val small = if(nbrs.length < nbrsOfNbrs.length) nbrs else nbrsOfNbrs
        val large = if(nbrs.length < nbrsOfNbrs.length) nbrsOfNbrs else nbrs
        val smallMax = nbrsMax 
        val largeMax = nbrsMax
        //I understand there are simplier ways to write this, I tried a lot of versions
        //this is the fastest (that I tried).
        var notFinished = small(i) < smallMax && large(j) < largeMax
        while(i < (small.length-1)  && j < (large.length-1) && notFinished){
          while(j < (large.length-1) && large(j) < small(i) && notFinished){
            j += 1
            notFinished = large(j) < largeMax
          }
          if(small(i)==large(j) && notFinished){
           t += 1
          }
          i += 1
          notFinished = notFinished && small(i) < smallMax
        }
        //if i reaches the end before j
        while(j < (large.length-1) && large(j) < small(i) && notFinished){
          j += 1
          notFinished = large(j) < largeMax
        }
        //if j reaches the end before i
        while(large(j) > small(i) && i < (small.length-1) && notFinished){
          i += 1
          notFinished = small(i) < smallMax
        }
        if(small(i) == large(j) && notFinished) t += 1 
        t
      }
      compiler ("ndv_intersect_gallop_in_range") ((("y",NodeDataView(T)),("max",T)) :: MLong, TNumeric(T)) implements single ${
        val x = $0
        var i = 0
        var j = 0
        var t = 0l
        var notFinished = y(j) < max && x(i) < max
        while (notFinished) {
          if (x(i) == y(j)) {
            t += 1
            i += 1
            j += 1
            notFinished = (i < x.length && j < y.length) && (x(i) < max) && (y(j) < max)
          }
          val arg2 = if(notFinished) x(i) < y(j) else false
          if (arg2) { 
            i = gallop_in_range(x,i,y(j),max)
            notFinished = (i < x.length) && (x(i) < max)
          }
          val arg3 = if(notFinished) x(i) > y(j) else false
          if(arg3){
            j = gallop_in_range(y,j,x(i),max)
            notFinished = (j < y.length) && (y(j) < max)
          }
        }
        t
      }
      compiler ("gallop_in_range") ( (("startIn",MInt),("tt",T),("max",T)) :: MInt, TNumeric(T)) implements single ${
        var start = startIn
        var stepSize = 1
        val v = $0
        var notFinished = (v(start) < tt && v(start) < max)
        var inRange = false
        while ((start < v.length) && notFinished) {
          notFinished = (v(start) < tt && v(start) < max)
          if(notFinished && ((start + stepSize) < v.length)) {
            if(v(start+stepSize) < tt){
              start += stepSize
              stepSize = stepSize << 1
            } else {
            start += 1
            stepSize = 1
            } 
          }
          else {
            start += 1
            stepSize = 1
          } 
        }
        if(!notFinished) start-1
        else start
      }
      infix ("intersect") (NodeDataView(T) :: MLong, TNumeric(T)) implements single ${
        val nbrs = $self
        val nbrsOfNbrs = $1
        if(nbrs.length == 0 || nbrsOfNbrs.length == 0) 0l
        else if(nbrs(0) > nbrsOfNbrs(nbrsOfNbrs.length-1) || 
          nbrsOfNbrs(0) > nbrs(nbrs.length-1)){
          0l
        }
        //else if(nbrs.length > 128 || nbrsOfNbrs.length > 128){
        //  $self.intersectGallop($1)
        //}
        else{
          ndv_intersect_sets(nbrs,nbrsOfNbrs)
        }
      }
      compiler ("ndv_intersect_sets") (NodeDataView(T) :: MLong, TNumeric(T)) implements single ${
        val nbrs = $self
        val nbrsOfNbrs = $1
        var i = 0
        var t = 0l
        var j = 0
        val small = if(nbrs.length < nbrsOfNbrs.length) nbrs else nbrsOfNbrs
        val large = if(nbrs.length < nbrsOfNbrs.length) nbrsOfNbrs else nbrs
        //I understand there are simplier ways to write this, I tried a lot of versions
        //this is the fastest (that I tried).
        while(i < (small.length-1)  && j < (large.length-1)){
          while(j < (large.length-1) && large(j) < small(i)){
            j += 1
          }
          if(small(i)==large(j)){
           t += 1
          }
          i += 1
        }
        //if i reaches the end before j
        while(j < (large.length-1) && large(j) < small(i)){
          j += 1
        }
        //if j reaches the end before i
        while(large(j) > small(i) && i < (small.length-1)){
          i += 1
        }
        if(small(i) == large(j)) t += 1 
        t
      }
      infix ("serialForeach") ((T ==> MUnit) :: MUnit, effect = simple) implements single ${
        var i = 0
        while(i < $self.length){
          $1($self(i))
          i += 1
        }
      }      
      infix ("print") (Nil :: MUnit, effect = simple) implements single ${
        var i = 0
        while(i < $self.length){
          println("NodeDataView -- Index: " + i + " Data: " + $self(i))
          i += 1
        }
      }
      infix ("getRawArray") (Nil :: MArray(T)) implements composite ${
        val d = array_empty[T]($self.length)
        array_copy(NodeDataView_data($self),NodeDataView_start($self),d,0,$self.length)
        d
      }

      compiler ("NodeDataView_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("NodeDataView_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("NodeDataView_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("NodeDataViews cannot be allocated from a parallel op") }
      compiler ("NodeDataView_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite ${ fatal("NodeDataViews cannot be updated") }
      parallelize as ParallelCollection(T, lookupOp("NodeDataView_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("NodeDataView_illegalupdate"))
    }
    compiler (NodeData) ("ndv_fake_alloc", Nil, Nil :: NodeDataView(MInt)) implements single ${ NodeDataView(array_empty_imm[Int](0),0,0) }
    direct(NodeDataView) ("sumOverCollection", (T,R), CurriedMethodSignature(List(("nd_view",NodeDataView(T)), ("data",T==>R) ,("cond",T==>MBoolean)),R), TNumeric(R)) implements composite ${nd_view.mapreduce[R]( e => data(e), (a,b) => a+b, cond)}
  }
}
