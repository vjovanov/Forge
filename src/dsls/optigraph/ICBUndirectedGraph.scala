/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all ICBUndirectedGraph operations.  Glues 
togther all structures and declares ICBUndirectedGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}
trait ICBUndirectedGraphOps{
  this: OptiGraphDSL =>
  def importICBUndirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val GraphBitSet = lookupTpe("GraphBitSet")
    val HashSet = lookupTpe("HashSet")
    val NodeCollection = lookupTpe("NodeCollection")
    //Actual ICBUndirectedGraph declaration
    val ICBUndirectedGraph = tpe("ICBUndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")

    data(ICBUndirectedGraph,("_numNodes",MInt),("_numEdges",MInt),("_externalIDs",MArray(MInt)),("_numHash",MInt),("_numCSR",MInt),("_numBitSet",MInt),("_hashNeighbors",MArray(HashSet(MInt))),("_csrNodes",MArray(MInt)),("_csrEdges",MArray(MInt)),("_bsNeighbors",MArray(GraphBitSet))) 
    static(ICBUndirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("numEdges",MInt),("externalIDs",MArray(MInt)),("numHash",MInt),("numCSR",MInt),("numBitSet",MInt),("hashNeighbors",MArray(HashSet(MInt))),("csrNodes",MArray(MInt)),("csrEdges",MArray(MInt)),("bsNeighbors",MArray(GraphBitSet))), ICBUndirectedGraph))) implements allocates(ICBUndirectedGraph,${numNodes},${numEdges},${externalIDs},${numHash},${numCSR},${numBitSet},${hashNeighbors},${csrNodes},${csrEdges},${bsNeighbors})

    val ICBUndirectedGraphOps = withTpe(ICBUndirectedGraph)     
    ICBUndirectedGraphOps{
      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      infix ("numEdges")(Nil :: MInt) implements getter(0,"_numEdges")
      infix ("numCSR")(Nil :: MInt) implements getter(0,"_numCSR")
      infix ("numHash")(Nil :: MInt) implements getter(0,"_numHash")
      infix ("numBitSet")(Nil :: MInt) implements getter(0,"_numBitSet")

      infix ("countTriangles") ( Nil :: MLong) implements composite ${
        $self.sumOverNodes{ n =>
          if(n.id >= ($self.numBitSet+$self.numCSR)){
            val nbrs = icb_hash_apply($self,n.id-($self.numBitSet+$self.numCSR))

            sumOverCollection(nbrs){ nbr => //type lookup
              if(nbr > n.id){
                if(nbr >= ($self.numBitSet+$self.numCSR)){ 
                  intersect(n.id,nbrs,nbr,icb_hash_apply($self,nbr-($self.numBitSet+$self.numCSR)))
                }
                else if(nbr >= $self.numBitSet){
                  intersect(n.id,nbrs,nbr,icb_get_csr_nbrs($self,nbr-$self.numBitSet))
                }
                else{
                  intersect(n.id,nbrs,nbr,icb_bs_apply($self,nbr))
                }
              } 
              else 0l
            }{e => true}
          }
          else if(n.id >= $self.numBitSet){
            val nbrs = icb_get_csr_nbrs($self,n.id-$self.numBitSet)

            sumOverCollection(nbrs){ nbr => //type lookup
              if(nbr > n.id){
                if(nbr >= ($self.numBitSet+$self.numCSR)){ 
                  intersect(n.id,nbrs,nbr,icb_hash_apply($self,nbr-($self.numBitSet+$self.numCSR)))
                }
                else if(nbr >= $self.numBitSet){
                  intersect(n.id,nbrs,nbr,icb_get_csr_nbrs($self,nbr-$self.numBitSet))
                }
                else{
                  intersect(n.id,nbrs,nbr,icb_bs_apply($self,nbr))
                }
              } 
              else 0l
            }{e => true}
          }
          else{
            val nbrs = icb_bs_apply($self,n.id)
            
            sumOverCollection(nbrs){ nbr => //type lookup
              if(nbr > n.id){
                if(nbr >= ($self.numBitSet+$self.numCSR)){ 
                  intersect(n.id,nbrs,nbr,icb_hash_apply($self,nbr-($self.numBitSet+$self.numCSR)))
                }
                else if(nbr >= $self.numBitSet){
                  intersect(n.id,nbrs,nbr,icb_get_csr_nbrs($self,nbr-$self.numBitSet))
                }
                else{
                  intersect(n.id,nbrs,nbr,icb_bs_apply($self,nbr))
                }
              } 
              else 0l
            }{e => true}
          }
        }
      }

      infix ("neighbors") (Node :: NodeCollection) implements single ${icb_get_neighbors($self,$1.id)} 
      infix ("neighbors") (MInt :: NodeCollection) implements single ${icb_get_neighbors($self,$1)}
      infix ("commonNeighborsOf") ( (MInt,MInt) :: MLong) implements single ${
        0l
        /*
        println("getting common neighbors: " + $1 + " " + $2)
        val bitset1 = $1 < $self.numBitSet
        val csr1 = $1 < ($self.numBitSet+$self.numCSR)
        val hash1 = !(bitset1 || csr1)

        val bitset2 = $2 < $self.numBitSet
        val csr2 = $2 < ($self.numBitSet+$self.numCSR)
        val hash2 = !(bitset2 || csr2)
      
        if (hash1 && hash2){  ///hash & hash
          println("hash")
          0l
          //intersect($1,icb_hash_apply($self,$1-($self.numBitSet+$self.numCSR)),$2,icb_hash_apply($self,$2-($self.numBitSet+$self.numCSR)))
        } else { //bs & csr
          val a = icb_bs_apply($self,$1)
          println("csr and bs 1: " + a.length)
          a.length.toLong
        } else if(csr1 && csr2){ //csr & csr
          println("csr")
          intersect($1,icb_get_csr_nbrs($self,$1-$self.numBitSet),$2,icb_get_csr_nbrs($self,$2-$self.numBitSet))
        } else if(hash1 && csr2){  //hash & csr
          println("hash and csr")
          intersect($1,icb_hash_apply($self,$1-($self.numBitSet+$self.numCSR)),$2,icb_get_csr_nbrs($self,$2-$self.numBitSet))
        } else if(csr1 && hash2){
          println("csr and hash")
          intersect($2,icb_hash_apply($self,$2-($self.numBitSet+$self.numCSR)),$1,icb_get_csr_nbrs($self,$1-$self.numBitSet))
        } else if(hash1 && bitset2){ //hash & bs
          println("hash and bs")
          intersect($1,icb_hash_apply($self,$1-($self.numBitSet+$self.numCSR)),$2,icb_bs_apply($self,$2))
        } else if(hash2 && bitset1){
          println("hash and bs 2")
          val 
          intersect($2,icb_hash_apply($self,$2-($self.numBitSet+$self.numCSR)),$1,icb_bs_apply($self,$1))
        } else if(bitset1 && csr2){ //bs & csr
          val a = icb_bs_apply($self,$1)
          println("csr and bs 1: " + a)
          0l 
          //intersect($1,icb_bs_apply($self,$1),$2,icb_get_csr_nbrs($self,$2-$self.numBitSet))
        }  else if(bitset1 && bitset2){ //bs & bs
          println("bs and bs")
          intersect($1,icb_bs_apply($self,$1),$2,icb_bs_apply($self,$2))
        }
        else { /*if(csr1 && bitset2)*/
          println("csr and bs 2")
          intersect($2,icb_bs_apply($self,$2),$1,icb_get_csr_nbrs($self,$1-$self.numBitSet))
        }
        */
      }
      compiler("icb_get_neighbors")(MInt :: NodeCollection) implements single ${
        if($1 < $self.numBitSet) NodeCollection(icb_bs_apply($self,$1))
        else if($1 < ($self.numBitSet+$self.numCSR)) NodeCollection(icb_get_csr_nbrs($self,$1-$self.numBitSet))
        else NodeCollection(icb_hash_apply($self,$1-($self.numBitSet+$self.numCSR)))
      }

    }
    addHyperUndirectedGraphCommonOps(ICBUndirectedGraph)
  } 
}