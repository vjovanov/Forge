package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

// This object lets us build our DSL
object OptiGraphDSLRunner extends ForgeApplicationRunner with OptiGraphDSL

trait OptiGraphDSL extends ForgeApplication with GraphOps with ForgeTrieBitmapOps
    with HABUndirectedGraphOps  with CSRDirectedGraphOps  with CSRUndirectedGraphOps
    with NodeOps with EdgeOps  with NodeDataOps with NodeDataViewOps with CSRBigUndirectedGraphOps with RoaringUndirectedGraphOps
    with NodeIdViewOps  with AtomicIntArrayOps with AtomicBooleanOps with ForgeRoaringBitmapOps
    with GraphBitSetOps with NodeCollectionOps with IOGraphOps with SparseUndirectedGraphOps {
  /**
   * The name of our DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  override def dslName = "OptiGraph"
    
  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  override def specification() = {
    /**
     * Include Scala ops
     */
    importScalaOps()
    importBitSetOps()
    importHashSetOps()
    importForgeRoaringBitmapOps()
    importForgeTrieBitmapOps()
    /**
     * The main portion of our DSL
     */
    importNodeOps()
    importEdgeOps()
    importNodeDataOps()
    importAtomicBooleanOps()
    importAtomicIntArrayOps()
    importNodeDataViewOps()
    importNodeIdViewOps()
    importGraphBitSetOps()
    importNodeCollectionOps()
    importGraphAggregateOps()
    importCSRUndirectedGraphOps()
    importCSRDirectedGraphOps()
    importCSRBigUndirectedGraphOps()
    importRoaringUndirectedGraphOps()
    importSparseUndirectedGraphOps()
    importHABUndirectedGraphOps()
    importIOGraphOps()
  }
} 
