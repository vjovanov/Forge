package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

// For compiler (Delite) implementation
trait ForgeArrayNumaOpsExp extends DeliteArrayFatExp {
  this: DeliteOpsExp /*with ForgeHashMapOpsExp*/ =>

  type ForgeArrayNuma[T] = DeliteArrayNuma[T]
  implicit def forgeArrayNumaManifest[T:Manifest] = manifest[DeliteArrayNuma[T]]

  def array_numa_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
    = darray_numa_empty[T](__arg0, __arg0)
  // def array_numa_empty_imm[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = darray_new_immutable[T](__arg0)
  // def array_copy[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArrayNuma[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  //   = darray_copy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_numa_update[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_update(__arg0,__arg1,__arg2)
  def array_numa_apply[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = darray_apply(__arg0,__arg1)
  def array_numa_length[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[Int]
    = darray_length(__arg0)
  // def array_clone[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]] = {
  //   val out = darray_new[T](darray_length(__arg0))
  //   darray_copy(__arg0, unit(0), out, unit(0), darray_length(__arg0))
  //   delite_unsafe_immutable(out)
  // }
  // def array_take[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[Int]): Rep[ForgeArrayNuma[T]]
  //   = darray_take(__arg0,__arg1)
  // def array_mkstring[A:Manifest](__arg0: Rep[ForgeArrayNuma[A]],__arg1: Rep[String])(implicit __imp0: SourceContext): Rep[String]
  //   = darray_mkstring(__arg0,__arg1)
  // def array_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  //   = darray_map(__arg0,__arg1)
  // //def array_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]], __arg1: Rep[T] => Rep[ForgeArrayNuma[R]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  // //  = darray_flatmap(__arg0,__arg1)
  // def array_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[ForgeArrayNuma[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[R]]
  //   = darray_zipwith(__arg0,__arg1,__arg2)
  // def array_reduce[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
  //   = darray_reduce(__arg0,__arg1,__arg2)
  // def array_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]]
  //   = darray_groupByReduce(__arg0,__arg1,__arg2,__arg3)
  // def array_filter[T:Manifest](__arg0: Rep[ForgeArrayNuma[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = darray_filter(__arg0,__arg1)
  // def array_sort[T:Manifest:Ordering](__arg0: Rep[ForgeArrayNuma[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = darray_sort(__arg0)
  // def array_fromfunction[T:Manifest](__arg0: Rep[Int],__arg1: Rep[Int] => Rep[T])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]]
  //   = darray_fromfunction(__arg0,__arg1)
  // def array_fromseq[T:Manifest](__arg0: Seq[Rep[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[T]] = {
  //   val out = darray_new[T](unit(__arg0.length))
  //   for (i <- 0 until __arg0.length) {
  //     out(unit(i)) = __arg0(i)
  //   }
  //   delite_unsafe_immutable(out)
  // }
  def array_numa_string_split(__arg0: Rep[String],__arg1: Rep[String],__arg2: Rep[Int] = unit(0))(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[String]]
    = reflectPure(ArrayNumaStringSplit(__arg0, __arg1, __arg2))
  // def array_sortIndices[R:Manifest:Ordering](__arg0: Rep[Int], __arg1: (Rep[Int] => Rep[R]))(implicit __imp0: SourceContext): Rep[ForgeArrayNuma[Int]]
  //   = darray_sortIndices(__arg0,{(a,b) =>
  //       val aV = __arg1(a)
  //       val bV = __arg1(b)

  //       // You have to have 3 conditions and then a default.
  //       // Otherwise you will violate the Java runtime comparator contract.
  //       if (delite_less_than(aV, bV)) unit(-1)
  //       else if (delite_equals(aV, bV)) unit(0)
  //       else if (delite_greater_than(aV, bV)) unit(1)
  //       else unit(0)
  //     })

  // avoid mixing in LMS Array ops due to conflicts. alternatively, we could refactor LMS array ops to
  // put ArrayApply and ArrayLength in an isolated trait that we can use.
  case class ArrayNumaApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  case class ArrayNumaLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayNumaStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int]) extends Def[DeliteArrayNuma[String]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayNumaApply(a,x) => scala_array_numa_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case ArrayNumaLength(a) => scala_array_numa_length(f(a))(mtype(manifest[A]),pos)
    case ArrayNumaStringSplit(a,b,l) => array_numa_string_split(f(a),f(b),f(l))(pos)

    case Reflect(ArrayNumaApply(a,x), u, es) => reflectMirrored(Reflect(ArrayNumaApply(f(a),f(x))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ArrayNumaLength(a), u, es) => reflectMirrored(Reflect(ArrayNumaLength(f(a))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ArrayNumaStringSplit(a,b,l), u, es) => reflectMirrored(Reflect(ArrayNumaStringSplit(f(a),f(b),f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  def scala_array_numa_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = ArrayNumaApply(__arg0,__arg1)
  def scala_array_numa_length[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int]
    = ArrayNumaLength(__arg0)

  /* NUMA-aware arrays */
  // def array_numa_empty[T:Manifest](len: Rep[Int]): Rep[ForgeArrayNuma[T]]
  //   = darray_numa_empty[T](len, len)
  def array_numa_combine_average[T:Manifest](x: Rep[ForgeArrayNuma[T]]): Rep[Unit]
    = darray_numa_combine_average[T](x)
  def array_numa_initial_synch[T:Manifest](x: Rep[ForgeArrayNuma[T]]): Rep[Unit]
    = darray_numa_initial_synch[T](x)
}

trait ScalaGenForgeArrayNumaOps extends ScalaGenDeliteArrayOps {
  val IR: ForgeArrayNumaOpsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayNumaApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case ArrayNumaLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    // TODO
    case ArrayNumaStringSplit(a,b,l) if Config.generateSerializable => emitValDef(sym, "new ppl.delite.runtime.data.LocalDeliteArrayObject[String](" + quote(a) + ".split(" + quote(b) + "))")
    case ArrayNumaStringSplit(a,b,l) => emitValDef(sym, quote(a) + ".split(" + quote(b) + ", " + quote(l) + ")")
    case _ => super.emitNode(sym,rhs)
  }
}
trait CLikeGenForgeArrayNumaOps extends CLikeGenBase {
  val IR: ForgeArrayNumaOpsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayNumaApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case ArrayNumaLength(x) => emitValDef(sym, quote(x) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenForgeArrayNumaOps extends CudaGenDeliteArrayOps with CLikeGenForgeArrayNumaOps { val IR: ForgeArrayNumaOpsExp with DeliteOpsExp }
trait OpenCLGenForgeArrayNumaOps extends OpenCLGenDeliteArrayOps with CLikeGenForgeArrayNumaOps { val IR: ForgeArrayNumaOpsExp with DeliteOpsExp }
trait CGenForgeArrayNumaOps extends CGenDeliteArrayOps {
  val IR: ForgeArrayNumaOpsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayNumaApply(x,n) => emitValDef(sym, quote(x) + "->apply(" + quote(n) + ")")
    case ArrayNumaLength(x) => emitValDef(sym, quote(x) + "->length")
    //TODO: enable ArrayStringSplit in cluster mode
    case ArrayNumaStringSplit(a,b,l) if (!Config.generateSerializable) => emitValDef(sym, "string_split(resourceInfo," + quote(a) + "," + quote(b) + "," + quote(l) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}