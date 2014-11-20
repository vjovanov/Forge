package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait ForgeArrayNumaWrapper extends HUMAN_DSL_NAMEBase {
  // this: ForgeHashMapWrapper =>

  type ForgeArrayNuma[T] = scala.Array[T]
  implicit def forgeArrayNumaManifest[T:Manifest] = manifest[Array[T]]

  // def farray_from_sarray[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = __arg0
  def array_numa_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
    = new ForgeArrayNuma[T](__arg0)
  // def array_empty_imm[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = array_empty[T](__arg0)
  // def array_copy[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArrayNuma[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  //   = System.arraycopy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_numa_update[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = __arg0(__arg1) = __arg2
  def array_numa_apply[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = __arg0(__arg1)
  def array_numa_length[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[Int]
    = __arg0.length
  // def array_clone[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = __arg0.clone
  // def array_take[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int]): Rep[ForgeArrayNuma[T]]
  //   = __arg0.take(__arg1)
  // def array_mkstring[A:Manifest](__arg0: Rep[ForgeArrayNuma[A]],__arg1: Rep[String])(implicit __imp0: SourceContext): Rep[String]
  //   = __arg0.mkString(__arg1)
  // def array_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  //   = __arg0.map(__arg1)
  // //def array_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]], __arg1: Rep[T] => Rep[ForgeArrayNuma[R]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  // //  = __arg0.flatMap(__arg1.toSeq)
  // def array_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[ForgeArrayNuma[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  //   = __arg0.zip(__arg1).map(t => __arg2(t._1,t._2))
  // def array_reduce[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
  //   = if (array_length(__arg0) == 0) __arg2 else __arg0.reduce(__arg1)
  // def array_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]] = {
  //   val grp = __arg0.groupBy[K](__arg1)
  //   val hm = scala.collection.mutable.HashMap[K,V]()
  //   grp.foreach{ a =>
  //     hm.put(a._1,array_map(a._2,__arg2).reduce(__arg3))
  //   }
  //   hm
  // }
  // def array_filter[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = __arg0.filter(__arg1)
  // def array_sort[T:Manifest:Ordering](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]] = {
  //   val d = array_empty[T](__arg0.length)
  //   array_copy(__arg0,0,d,0,__arg0.length)
  //   scala.util.Sorting.quickSort(d)
  //   d
  // }
  // def array_sortIndices[R:Manifest:Ordering](__arg0: Rep[Int], __arg1: (Rep[Int] => Rep[R]))(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[Int]]
  //   = array_empty[Int](__arg0).indices.toArray.sortBy(__arg1)
  // def array_fromfunction[T:Manifest](__arg0: Rep[Int],__arg1: Rep[Int] => Rep[T])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]] = {
  //   Array.tabulate[T](__arg0)(__arg1)
  // }
  // def array_fromseq[T:Manifest](__arg0: Seq[Rep[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = __arg0.toArray
  // def array_string_split(__arg0: Rep[String],__arg1: Rep[String],__arg2: Rep[Int] = unit(0))(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[String]]
  //   = __arg0.split(__arg1,__arg2)

  def scala_array_numa_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = array_numa_apply(__arg0,__arg1)
  def scala_array_numa_length[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int]
    = array_numa_length(__arg0)

  // def array_numa_empty[T:Manifest](len: Rep[Int]): Rep[ForgeArrayNuma[T]]
  //   = new ForgeArrayNuma[T](len)
  def array_numa_combine_average[T:Manifest](x: Rep[ForgeArrayNuma[T]]): Rep[Unit]
    = {}
  def array_numa_initial_synch[T:Manifest](x: Rep[ForgeArrayNuma[T]]): Rep[Unit]
    = {}
}


