package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait IndexVectorOps {
  this: OptiLADSL =>

  def importIndexVectorOps() {
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")

    // data fields - we take a tagged union approach to enable range- and sequence- based IndexVectors without subtyping
    data(IndexVector, ("_data", MArray(MLong)), ("_start", MLong), ("_end", MLong), ("_isRow", MBoolean), ("_isRange", MBoolean))

    // static methods
    static (IndexVector) ("apply", Nil, (MLong,MLong) :: IndexVector) implements redirect ${ IndexVector($0,$1,unit(true)) }
    static (IndexVector) ("apply", Nil, (MLong,MLong,MBoolean) :: IndexVector) implements
      allocates(IndexVector, ${ array_empty_imm[Int](unit(0)) }, quotedArg(0), quotedArg(1), quotedArg(2), ${ unit(true) })

    static (IndexVector) ("apply", Nil, DenseVector(MLong) :: IndexVector) implements redirect ${ IndexVector($0,$0.isRow) }
    static (IndexVector) ("apply", Nil, (DenseVector(MLong), MBoolean) :: IndexVector) implements
      allocates(IndexVector, ${ indexvector_copyarray($0) }, ${ unit(0) }, ${ unit(0) }, quotedArg(1), ${ unit(false) })

    compiler (IndexVector) ("indexvector_fromarray", Nil, (MArray(MLong),MBoolean) :: IndexVector) implements
      allocates(IndexVector, quotedArg(0), ${ unit(0) }, ${ unit(0) }, quotedArg(1), ${ unit(false) })

    compiler (IndexVector) ("indexvector_copyarray", Nil, DenseVector(MLong) :: MArray(MLong)) implements composite ${
      val d = array_empty[Int]($0.length)
      $0.indices foreach { i => d(i) = $0(i) }
      d.unsafeImmutable
    }

    // this is unsafe because it uses the underlying input array directly instead of copying
    // they should only be used if we know the intermediate reference is dead or immutable (to avoid unsafe aliasing)
    // TODO: for some reason this does not work when we think it should, so we are reverting to the safer copy-always policy.
        
    // compiler (IndexVector) ("unsafe_dense_to_index", Nil, DenseVector(MLong) :: IndexVector) implements composite ${
    //   indexvector_fromarray(densevector_raw_data($0), $0.isRow)
    // }
    
    // index helpers
    for (arity <- 2 to 6) {
      val Tup = tpeInst(lookupTpe("Tuple"+arity, stage = compile), (0 until arity).map(i => MLong).toList)

      // unroll during staging to specialize for each arity
      val d = (2 to arity).map(k => "dims._" + k)
      val s1 = d.scanRight("1")((a,b) => a + "*" + b)

      val s2 = s1.zipWithIndex.map(t => "inds._"+(t._2+1) + "*" + t._1)
      val retFlat = s2.mkString(" + ")
      // e.g. for index (i,j,k,l) and dims (a,b,c,d), returns (i*b*c*d + j*c*d + k*d + l)
      direct (IndexVector) ("flatten", Nil, (("inds",Tup),("dims",Tup)) :: MLong) implements redirect ${ \$retFlat }

      val s3 = s1.zipWithIndex.map(t => "(i / (" + t._1 + ")) % dims._" + (t._2+1))
      val retTuple = s3.mkString("(",",",")")
      // e.g. for index i and dims (a,b,c,d), returns [i/dcb % a, i/dc % b, i/d % c, i/1 % d]
      direct (IndexVector) ("unflatten", Nil, (("i",MLong),("dims",Tup)) :: Tup) implements redirect ${ \$retTuple }
    }

    val IndexVectorOps = withTpe(IndexVector)
    IndexVectorOps {
      compiler ("indexvector_start") (Nil :: MLong) implements getter(0, "_start")
      compiler ("indexvector_end") (Nil :: MLong) implements getter(0, "_end")
      compiler ("indexvector_raw_data") (Nil :: MArray(MLong)) implements getter(0, "_data")
      compiler ("indexvector_is_range") (Nil :: MBoolean) implements getter(0, "_isRange")

      // TODO: the _isRange field should be a compile-time constant. can this be optimized (or does it already) eliminate the conditional in length/apply?

      infix ("length") (Nil :: MLong) implements composite ${
        if (indexvector_is_range($self)) {
          indexvector_end($self) - indexvector_start($self)
        }
        else {
          array_length(indexvector_raw_data($self))
        }
      }
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MLong :: MLong) implements composite ${
        if (indexvector_is_range($self)) {
          indexvector_start($self) + $1
        }
        else {
          indexvector_raw_data($self).apply($1)
        }
      }

      infix ("slice") ((("start",MLong),("end",MLong)) :: IndexVector) implements composite ${
        if (indexvector_is_range($self)) {
          fassert($start >= indexvector_start($self) && $end <= indexvector_end($self), "IndexVector slice (" + $start + "," + $end + ") out of bounds (" + indexvector_start($self) + "," + indexvector_end($self) + ")")
          IndexVector($start, $end, $self.isRow)
        }
        else {
          IndexVector($self.toDense.slice($start, $end))
        }
      }

      infix ("t") (Nil :: IndexVector) implements allocates(IndexVector, ${indexvector_raw_data($self)}, ${indexvector_start($self)}, ${indexvector_end($self)}, ${!(indexvector_isrow($self))}, ${indexvector_is_range($self)})

      infix ("Clone") (Nil :: IndexVector, aliasHint = copies(0)) implements composite ${
        if (indexvector_is_range($self)) {
          IndexVector(indexvector_start($self),indexvector_end($self),$self.isRow)
        }
        else {          
          indexvector_fromarray(array_clone(indexvector_raw_data($self)), $self.isRow)
        }
      }

      infix ("toDense") (Nil :: DenseVector(MLong)) implements composite ${ 
        if (indexvector_is_range($self)) { $self.map(e => e) }
        else {
          // this is safe because we are constructing an immutable DenseVector and IndexVectors are always immutable
          densevector_fromarray(indexvector_raw_data($0), $0.isRow)
        }        
      }

      direct ("__equal") (IndexVector :: MBoolean) implements composite ${ $self.toDense == $1 }
      direct ("__equal") (DenseVector(MLong) :: MBoolean) implements composite ${ $1 == $self }

      infix ("filter") ((MLong ==> MBoolean) :: IndexVector) implements composite ${ IndexVector($self.toDense.filter($1)) }

      // parallel, so the conversion can fuse with the consumer
      // is this fast and robust enough to capture parallel operators over index vectors?
      fimplicit ("indexToDense") (Nil :: DenseVector(MLong)) implements composite ${        
        if (Settings.verbose > 0) println("(performance warning): automatic conversion from IndexVector to DenseVector")
        $self.toDense
      }

      // naming is by convention here, a little brittle. would it be better to put this in extern?
      val grpName = if (Config.fastCompile) "$Flat" else "DenseVector"
      fimplicit ("chainIndexToDenseOps") (Nil :: ephemeralTpe(grpName+"DenseVectorOpsCls[Int]", stage = now)) implements composite ${
        repTo\${grpName}DenseVectorOpsCls(indexToDense($self))
      }
      fimplicit ("chainIndexToDenseIntOps") (Nil :: ephemeralTpe(grpName+"DenseVectorIntOpsCls", stage = now)) implements composite ${
        repTo\${grpName}DenseVectorIntOpsCls(indexToDense($self))
      }

      compiler ("indexvector_illegalalloc") (MLong :: MNothing) implements composite ${ fatal("IndexVectors cannot be allocated from a parallel op") }
      compiler ("indexvector_illegalupdate") ((MLong, MLong) :: MNothing) implements composite ${ fatal("IndexVectors cannot be updated") }

      // IndexVectors can't be mapped over, but they can be zipped with or reduced
      parallelize as ParallelCollection(MLong, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",4), lookupOp("indexvector_illegalupdate"))
    }

    // allows us to perform operations without converting to a DenseVector first
    addVectorCommonOps(IndexVector,MLong)
  }
}
