package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack

// Front-end
trait ForgeArrayNumaOps extends Base with OverloadHack {
  // this: ForgeHashMapOps =>

  /**
   * We use ForgeArrayNuma[T] instead of T so we don't get any subtle errors related to shadowing Array[T]
   */
  type ForgeArrayNuma[T]
  implicit def forgeArrayNumaManifest[T:Manifest]: Manifest[ForgeArrayNuma[T]]

  /**
   * Applications may need direct access to ForgeArrayNumas, if, for example, they use string fsplit
   * How do we allow DSLs to only optionally include the Array API for end users?
   */
  implicit class ForgeArrayNumaOps[T:Manifest](x: Rep[ForgeArrayNuma[T]]) {
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = array_numa_apply(x,n)
    def length = array_numa_length(x)
  }

  def array_numa_apply[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def array_numa_length[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[Int]

  // def array_numa_empty[T:Manifest](__arg0: Rep[Int]): Rep[ForgeArrayNuma[T]]
  def array_numa_combine_average[T:Manifest](x: Rep[ForgeArrayNuma[T]]): Rep[Unit]
  def array_numa_initial_synch[T:Manifest](x: Rep[ForgeArrayNuma[T]]): Rep[Unit]

  /* Required for apps to be able use 'args' */
  // implicit class ScalaArrayNumaOps[T:Manifest](x: Rep[Array[T]]) {
  //   def apply(n: Rep[Int])(implicit ctx: SourceContext) = scala_array_numa_apply(x,n)
  //   def length = scala_array_numa_length(x)
  // }
  // // the implicit class method 'length' is not working for an unknown reason, possibly related to the problem mentioned in ForgeArrayNumaCompilerOps below
  // // omitting SourceContext is a hacky way to avoid conflicts with Forge DSLs without using an arbitrary Overloaded parameter
  def infix_numa_length[T:Manifest](x: Rep[Array[T]])(implicit __imp0: SourceContext, o:Overloaded1) = scala_array_numa_length(x)
  def scala_array_numa_apply[T:Manifest](x: Rep[Array[T]], n: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def scala_array_numa_length[T:Manifest](x: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int]
}
trait ForgeArrayNumaCompilerOps extends ForgeArrayNumaOps {
  // this: ForgeHashMapCompilerOps =>

  /**
   * There are some unfortunate scalac typer crashes when we try to use the nicer front-end from DSLs :(
   */
  object ArrayNuma {
    def empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext) = array_numa_empty[T](__arg0)
    // def copy[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArrayNuma[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext) = array_copy(__arg0,__arg1,__arg2,__arg3,__arg4)
  }

  implicit class ForgeArrayNumaCompilerOps[T:Manifest](x: Rep[ForgeArrayNuma[T]]) {
    def update(n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext) = array_numa_update(x,n,y)    
    // def map[R:Manifest](f: Rep[T] => Rep[R])(implicit ctx: SourceContext) = array_map[T,R](x,f)
    // def Clone(implicit ctx: SourceContext) = array_clone(x)
  }

  // def farray_from_sarray[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  def array_numa_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  // def array_empty_imm[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  // def array_raw_alloc[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]] = array_empty[T](__arg1)
  // def array_copy[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArrayNuma[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  def array_numa_update[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
  // def array_clone[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  // def array_take[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int]): Rep[ForgeArrayNuma[T]]
  // def array_mkstring[A:Manifest](__arg0: Rep[ForgeArrayNuma[A]],__arg1: Rep[String])(implicit __imp0: SourceContext): Rep[String]
  // def array_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  //def array_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]], __arg1: Rep[T] => Rep[ForgeArrayNuma[R]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  // def array_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[ForgeArrayNuma[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  // def array_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]]
  // def array_reduce[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
  // def array_filter[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  // def array_sort[T:Manifest:Ordering](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  // def array_fromfunction[T:Manifest](__arg0: Rep[Int],__arg1: Rep[Int] => Rep[T])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  // def array_fromseq[T:Manifest](__arg0: Seq[Rep[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  // def array_string_split(__arg0: Rep[String],__arg1: Rep[String],__arg2: Rep[Int] = unit(0))(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[String]]
  // def array_sortIndices[R:Manifest:Ordering](__arg0: Rep[Int], __arg1: (Rep[Int] => Rep[R]))(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[Int]]

  def scala_array_numa_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def scala_array_numa_length[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int]
}