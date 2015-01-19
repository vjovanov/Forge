package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack

// Front-end
trait ForgeMultiArrayOps extends Base with OverloadHack {
  this: ForgeHashMapOps =>

  type ForgeMultiArray[T]
  type ForgeArray1D[T]
  type ForgeArray2D[T]
  type ForgeArray3D[T]
  type ForgeArray4D[T]
  type ForgeArray5D[T]

  // --- implicit manifests
  implicit def forgeMultiArrayManifest[T:Manifest]: Manifest[ForgeMultiArray[T]]
  implicit def forgeArray1DManifest[T:Manifest]: Manifest[ForgeArray1D[T]]
  implicit def forgeArray2DManifest[T:Manifest]: Manifest[ForgeArray2D[T]]
  implicit def forgeArray3DManifest[T:Manifest]: Manifest[ForgeArray3D[T]]
  implicit def forgeArray4DManifest[T:Manifest]: Manifest[ForgeArray4D[T]]
  implicit def forgeArray5DManifest[T:Manifest]: Manifest[ForgeArray5D[T]]

  // --- inheritance casts
  implicit def forgeArray1DToMultiArray[T:Manifest](ma: Rep[ForgeArray1D[T]]): Rep[ForgeMultiArray[T]]
  implicit def forgeArray2DToMultiArray[T:Manifest](ma: Rep[ForgeArray2D[T]]): Rep[ForgeMultiArray[T]]
  implicit def forgeArray3DToMultiArray[T:Manifest](ma: Rep[ForgeArray3D[T]]): Rep[ForgeMultiArray[T]]
  implicit def forgeArray4DToMultiArray[T:Manifest](ma: Rep[ForgeArray4D[T]]): Rep[ForgeMultiArray[T]]
  implicit def forgeArray5DToMultiArray[T:Manifest](ma: Rep[ForgeArray5D[T]]): Rep[ForgeMultiArray[T]]

  /**
   * Applications may need direct access to MultiArrays, if, for example, they use string fsplit
   * How do we allow DSLs to only optionally include the Array API for end users?
   */
  implicit class ForgeMultiArrayOps[T:Manifest](ma: Rep[ForgeMultiArray[T]]) {
    def apply(i: Rep[Int]*)(implicit ctx: SourceContext) = multia_apply(x,i)
    def size = multia_size(x)
  }
  def multia_apply[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[T]
  def multia_size[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Int]
}

trait ForgeMultiArrayCompilerOps extends ForgeMultiArrayOps {
  this: ForgeHashMapCompilerOps =>

  object MultiArray {
    def empty[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = multia_empty[T](dims)
  }

  implicit class ForgeMultiArrayCompilerOps[T:Manifest](ma: Rep[ForgeMultiArray[T]]) {
    // --- rank casts
    def as1D: Rep[ForgeArray1D[T]] = multia_as_1D(ma)
    def as2D: Rep[ForgeArray2D[T]] = multia_as_2D(ma)
    def as3D: Rep[ForgeArray3D[T]] = multia_as_3D(ma)
    def as4D: Rep[ForgeArray4D[T]] = multia_as_4D(ma)
    def as5D: Rep[ForgeArray5D[T]] = multia_as_5D(ma)

    def update(i: Seq[Rep[Int]], x: Rep[T])(implicit ctx: SourceContext) = multia_update(ma,i,x)    
    def map[R:Manifest](f: Rep[T] => Rep[R])(implicit ctx: SourceContext) = multia_map[T,R](ma,f)
    def Clone(implicit ctx: SourceContext) = multia_clone(ma)
  }

  // --- Rank type casts
  def multia_as_1D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray1D[T]]
  def multia_as_2D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray2D[T]]
  def multia_as_3D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray3D[T]]
  def multia_as_4D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray4D[T]]
  def multia_as_5D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray5D[T]]

  // --- Constructors
  def multia_empty[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_empty_imm[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_fromfunction[T:Manifest](dims: Seq[Rep[Int]], func: Rep[Seq[Rep[Int]]] => Rep[T])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  //def multia_from_sarray[T:Manifest](x: Rep[Array[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  
  def multia_view[T:Manifest](ma: Rep[ForgeMultiArray[T]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]]): Rep[ForgeMultiArray[T]]

  // --- Properties
  def multia_rank[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Int]
  def multia_shape[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Seq[Rep[Int]]]

  // --- Single element operators
  def multia_update[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]],x: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  
  // --- Copies / Mutating / Permuting
  def multia_clone[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_permute[T:Manifest](ma: Rep[ForgeMultiArray[T]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_reshape[T:Manifest](ma: Rep[ForgeMultiArray[T]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]

  // --- Parallel ops
  def multia_map[T:Manifest,R:Manifest](in: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  def multia_zipwith[T:Manifest,B:Manifest,R:Manifest](inA: Rep[ForgeMultiArray[T]],inB: Rep[ForgeMultiArray[B]], func: (Rep[T],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  def multia_reduce[T:Manifest](in: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T])(implicit ctx: SourceContext): Rep[T]

  def multia_mmap[T:Manifest](in: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def multia_mzipwith[T:Manifest](inA: Rep[ForgeMultiArray[T]], inB: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[Unit]

  // --- Bulk
  def multia_mkstring[T:Manifest](ma: Rep[ForgeMultiArray[T]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  
  // --- 1D Operations
  def multia_sort[T:Manifest:Ordering](ma: Rep[ForgeArray1D[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]] = {
    val indices = multia_sortIndices(ma.size, (i: Rep[Int]) => ma(i))
    indices.map{i => ma(i)}
  }
  def multia_sortwith[T:Manifest](ma: Rep[ForgeArray1D[T]], comp: (Rep[T],Rep[T]) => Rep[Int])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]] 
  def multia_sortIndices[R:Manifest:Ordering](len: Rep[Int], comp: (Rep[Int] => Rep[R]))(implicit ctx: SourceContext): Rep[ForgeArray1D[Int]]

  def multia_flatmap[T:Manifest,R:Manifest](in: Rep[ForgeArray1D[T]], func: Rep[T] => Rep[ForgeArray1D[R]])(implicit ctx: SourceContext): Rep[ForgeArray1D[R]]
  def multia_filter[T:Manifest](in: Rep[ForgeArray1D[T]], cond: Rep[T] => Rep[Boolean])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]
  def multia_groupBy[T:Manifest,K:Manifest](in: Rep[ForgeArray1D[T]], key: Rep[T] => Rep[K])(implicit ctx: SourceContext): Rep[ForgeMultiMap[K,ForgeArray1D[T]]]
  def multia_groupByReduce[T:Manifest,K:Manifest,V:Manifest](in: Rep[ForgeArray1D[T]],key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[ForgeMultiMap[K,V]]

  def multia_fromseq[T:Manifest](sq: Seq[Rep[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]
  def multia_string_split(str: Rep[String], pat: Rep[String], ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[ForgeArray1D[String]]

  // --- 2D Operations
  def multia_matmult[T:Manifest:Numeric](lhs: Rep[ForgeArray2D[T]], rhs: Rep[ForgeArray2D[T]])(implicit ctx: SourceContext): Rep[ForgeArray2D[T]]
  def multia_matvecmult[T:Manifest:Numeric](lhs: Rep[ForgeArray2D[T]], rhs: Rep[ForgeArray1D[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]
}