package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}
import factor._

trait MLIOOps {
  this: OptiMLDSL =>

  def importMLIOOps() {
    val IO = grp("MLio")

    val DenseVector = lookupTpe("DenseVector")
    val FactorGraph = lookupTpe("FactorGraph")
    val Weight = lookupTpe("Weight")
    val FactorVariable = lookupTpe("FactorVariable")
    val Variable = lookupTpe("RandomVariable")
    val FunctionFactor = lookupTpe("FunctionFactor")
    val SHashMap = lookupTpe("scala.collection.mutable.HashMap")

    // -- input

    // currently we only support DeepDive-style factor graphs
    direct (IO) ("readFactorGraph", Nil, MethodSignature(List(("factorsPath", MString), ("variablesPath", MString), ("weightsPath", MString), ("delim",MString,"\"\\t\"")), FactorGraph(FunctionFactor))) implements composite ${
      val weights = ForgeFileReader.readLines($weightsPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (id, initialValue, isFixed) = (tokens(0).toInt, tokens(1).toDouble, tokens(2).toBoolean)
        Weight(id, initialValue, isFixed)
      }
      val weightsMap = fhashmap_from_arrays(weights.map(w => w.id), weights)

      val variableRows = ForgeFileReader.readLines($variablesPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (variableId, factorId, position, isPositive, dataType, initialValue, isEvidence, isQuery) = (tokens(0).toInt, tokens(1).toInt, tokens(2).toInt, tokens(3).toBoolean, tokens(4), tokens(5).toDouble, tokens(6).toBoolean, tokens(7).toBoolean)
        pack((variableId, factorId, position, isPositive, dataType, initialValue, isEvidence, isQuery))
      }

      // currently only supporting boolean vars
      val variables = variableRows.map(r => RandomVariable(r._1, DenseVector(0.0, 1.0), r._6, r._7, r._8))
      // there are multiple variable entries per factor, so we need to remove duplicates
      val variablesMap = array_groupByReduce[RandomVariable,Int,RandomVariable](variables, v => v.id, v => v, (a,b) => a)
      val factorVariablesMap = densevector_fromarray(variableRows, true).groupBy(r => r._2, r => FactorVariable(r._1, r._4, DenseVector(0.0, 1.0)))

      // reading factors breaks in parallel if we try to look up the hashmap inside the file reading loop, for some reason
      val factorRows = ForgeFileReader.readLines($factorsPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (factorId, weightId, func) = (tokens(0).toInt, tokens(1).toInt, tokens(2))
        // val vars = if (factorVariablesMap.contains(factorId)) factorVariablesMap(factorId) else DenseVector[FactorVariable]()
        // FunctionFactor(factorId, vars, weightId, func)
        pack((factorId, weightId, func))
      }
      val factors = factorRows.map { t =>
        val vars = if (factorVariablesMap.contains(t._1)) factorVariablesMap(t._1) else DenseVector[FactorVariable]()
        FunctionFactor(t._1, vars, t._2, t._3)
      }

      val selectedFactors = array_filter[FunctionFactor](factors, f => f.vars.length > 0)
      val factorsMap = fhashmap_from_arrays(selectedFactors.map(f => f.id), selectedFactors)

      // build graph from weightsMap, variablesMap, factorsMap
      val variableFactorsMap = build_variable_factors_map(variablesMap, factorsMap)
      val variableValuesMap = chashmap_from_arrays(fhashmap_keys(variablesMap), fhashmap_values(variablesMap).map(v => v.value))
      val weightsValuesMap = chashmap_from_arrays(fhashmap_keys(weightsMap), fhashmap_values(weightsMap).map(w => w.value))

      FactorGraph(factorsMap, variablesMap, weightsMap, variableFactorsMap, variableValuesMap, weightsValuesMap)
    }


    // -- output



    // -- utility

    compiler (IO) ("build_variable_factors_map", Nil, (("variablesMap", MHashMap(MInt, Variable)), ("factorsMap", MHashMap(MInt, FunctionFactor))) :: MHashMap(MInt,DenseVector(MInt))) implements single ${
      // immutable, parallelizable, but slow
      // val variableFactorsValues = fhashmap_keys(variablesMap).map(vid => {
      //   // if factorsMap.variables contains id, then add that factor. this could be very slow! (O(|V|*|F|) while the mutable version was O(|V|+|F|))
      //   val factorIds = fhashmap_keys(factorsMap)
      //   densevector_fromarray(array_filter[Int](factorIds, fid => factorsMap(fid).vars.map(_.id).contains(vid)), true)
      // })
      // val variableFactorsMap = fhashmap_from_arrays(fhashmap_keys(variablesMap), variableFactorsValues)


      // mutable, sequential

      // using nested mutable maps violates the mutability rules and causes compiler errors, which should be spurious since the effects
      // are self-contained in this sequential block. However, it appears that using an immutable vector to hold the buckets does not
      // cost us much anyways.

      // val variableFactorsTempMap = SHashMap[Int,scala.collection.mutable.HashMap[Int,Int]]()
      val variableFactorsTempMap = SHashMap[Int,DenseVector[Int]]()

      val factorIds = fhashmap_keys(factorsMap)
      for (i <- 0 until array_length(factorIds)) {
        val f = factorsMap(factorIds(i))
        for (j <- 0 until f.vars.length) {
          val vId = f.vars.apply(j).id
          if (variableFactorsTempMap.contains(vId)) {
            //val curMap = variableFactorsTempMap(vId).unsafeMutable
            //curMap.update(factorIds(i), 1) // we're just using the map as a set, so the value doesn't matter
            val curVec = variableFactorsTempMap(vId)
            variableFactorsTempMap.update(vId, curVec << factorIds(i))
          }
          else {
            // val newMap = SHashMap[Int,Int]()
            // newMap.update(factorIds(i), 1)
            // variableFactorsTempMap.update(vId, newMap.unsafeImmutable)
            variableFactorsTempMap.update(vId, DenseVector(factorIds(i)))
          }
        }
      }

      val variableIds = fhashmap_keys(variablesMap)
      for (i <- 0 until array_length(variableIds)) {
        val v = variablesMap(variableIds(i))
        if (!variableFactorsTempMap.contains(v.id)) {
          // variableFactorsTempMap.update(v.id, SHashMap[Int,Int]().unsafeImmutable)
          variableFactorsTempMap.update(v.id, DenseVector[Int]())
        }
      }

      // val values = variableFactorsTempMap.values.map(m => densevector_fromarray(m.keys, true))
      val values = variableFactorsTempMap.values.map(v => v.distinct)
      fhashmap_from_arrays(variableFactorsTempMap.keys, values)
    }

    ()
  }
}
