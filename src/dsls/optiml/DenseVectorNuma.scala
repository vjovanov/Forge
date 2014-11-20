package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseVectorNumaOps {
  this: OptiMLDSL =>

  def importDenseVectorNumaOps() {
    val T = tpePar("T")
    val DenseVectorNuma = tpe("DenseVectorNuma", T)
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")

    data(DenseVectorNuma, ("_length", MInt), ("_isRow", MBoolean), ("_data", MArrayNuma(T)))

    static (DenseVectorNuma) ("apply", T, (MInt, MBoolean) :: DenseVectorNuma(T), effect = mutable) implements allocates(DenseVectorNuma, ${$0}, ${$1}, ${array_numa_empty[T]($0)})

    // how do we add Ordering[Variable] in Forge? no way in Forge to get Ordering(Variable) now, since Ordering is built-in
    // currently we can work-around this using sortBy, but this should really be possible in general..
    
    // val VariableOrdering = tpeClassInst("OrderingVariable", T withBound TOrdering, Ordering(Variable))
    // infix (DenseVectorArith) ("zero", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ DenseVector[T]($0.length,$0.isRow).unsafeImmutable }
    // infix (DenseVectorArith) ("empty", T withBound TArith, Nil :: DenseVector(T)) implements composite ${ DenseVector[T](unit(0),unit(true)).unsafeImmutable }
    
    val DenseVectorNumaOps = withTpe (DenseVectorNuma)
    DenseVectorNumaOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("data") (Nil :: MArrayNuma(T)) implements getter(0, "_data")
      infix ("apply") (MInt :: T) implements composite ${ array_numa_apply($self.data, $1) }
      infix ("apply") (IndexVector :: DenseVector(T)) implements composite ${
        val out = $1.map(i => $self(i))
        if ($self.isRow != $1.isRow) out.t else out // preserve orientation of original vector
      }
      infix ("update") ((("i",MInt),("e",T)) :: MUnit, effect = write(0)) implements composite ${
        array_numa_update($self.data, $i, $e)
      }

      infix ("update") ((("indices",IndexVector),("e",T)) :: MUnit, effect = write(0)) implements composite ${
        (0::indices.length) foreach { i =>
          fassert(indices(i) >= 0 && indices(i) < $self.length, "index out of bounds: bulk vector update")
          array_numa_update($self.data, indices(i), e)
        }
      }

      infix ("update") ((("indices",IndexVector),("v",DenseVector(T))) :: MUnit, effect = write(0)) implements single ${
        fassert(indices.length == v.length, "dimension mismatch: bulk vector update")

        // cannot be parallel unless indices contains only disjoint indices (why is why we use 'single' here)
        // however, maybe this should be a property that we guarantee of all IndexVectors
        (0::indices.length) foreach { i =>
          fassert(indices(i) >= 0 && indices(i) < $self.length, "index out of bounds: bulk vector update")
          array_numa_update($self.data, indices(i), v(i))
        }
      }
      infix ("combineAvg") (Nil :: MUnit, effect = write(0)) implements single ${
        array_numa_combine_average[T]($self.data)
      }
      infix ("initialSynch") (Nil :: MUnit, effect = write(0)) implements single ${
        array_numa_initial_synch[T]($self.data)
      }
    }
  }
}
