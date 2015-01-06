import optiml.compiler._
import optiml.library._
import optiml.shared._

object GibbsCompiler extends OptiMLApplicationCompiler with Gibbs
object GibbsInterpreter extends OptiMLApplicationInterpreter with Gibbs

trait Gibbs extends OptiMLApplication {

  def print_usage = {
    println("Usage: Gibbs <meta file> <factors file> <variables file> <weights file> <edges file>")
    exit(-1)
  }

  /* Samples a variable and updates its value in the graph */
  def sampleVariable(graph: Rep[FactorGraph], variableId: Rep[Int]) = { 
    //version 4
    
    val variable = graph.variables.apply(variableId)
    val (variableIStart, nFactors) = (variable.iStart, variable.nFactors)
    val factorsToVariables = graph.factorsToVariables
    val values = graph.variableValues
    
    val allSum = sum(0, nFactors) { i =>
      val factor = graph.variablesToFactors.apply(i + variableIStart)
      val positive = factor.evaluate(values, factorsToVariables, variableId, 1.0)
      val negative = factor.evaluate(values, factorsToVariables, variableId, 0.0)
      val factorWeightValue = graph.getWeightValue(factor.weightId)
      (negative - positive) * factorWeightValue
    }
    
    //val start = time(allSum)
    //val (positiveSum, negativeSum) = (allValues.map(_._1).sum, allValues.map(_._2).sum)
    val newValue = if ((random[Double] * (1.0 + exp(allSum))) <= 1.0) 1.0 else 0.0
    //val newValue = if ((0.5 * (1.0 + exp(allSum))) <= 1.0) 1.0 else 0.0
    //val end = time(newValue)
    val z = graph.updateVariableValue(variableId, newValue)
    newValue
  }

  /* Samples multiple variables and updates the variable values in the graph */
  def sampleVariables(graph: Rep[FactorGraph], variableIds: Rep[DenseVectorNuma[Int]], times: Rep[DenseVector[Tup2[Int,Long]]]) = {
    val start = time()
    val z = for (v <- (0::variableIds.length)) {
      val value = sampleVariable(graph, variableIds.apply(v))
    }
    val end = time()
    times <<= pack(variableIds.length, end - start)
  }

  def evaluateFactor(graph: Rep[FactorGraph], factorId: Rep[Int]) = {
    val factor = graph.factors.apply(factorId)
    val values = graph.variableValues
    val factorsToVariables = graph.factorsToVariables
    val variableId = factorsToVariables(factor.iStart).id
    factor.evaluate(values, factorsToVariables, variableId, values(variableId))
    // val factorVariableValues = factor.vars.map(fv => graph.getVariableValue(fv.id, fv.isPositive))
    // factor.evaluate(factorVariableValues)
  }

  // // computes the marginal probability that each factor is true
  // def sampleFactors(graph: Rep[FactorGraph], variableIds: Rep[DenseVector[Int]], factorIds: Rep[DenseVector[Int]], numSamples: Rep[Int], times: Rep[DenseVector[Tup2[Int,Long]]]) = {
  //   var i = 0
  //   val acc = DenseVector[Double](factorIds.length, true)
  //   while (i < numSamples) {
  //     // sample all variables and update their values
  //     sampleVariables(graph, variableIds, times)
  //     acc += factorIds.map(fid => evaluateFactor(graph, fid))
  //     i += 1
  //   }
  //   val res = acc / numSamples
  //   //factorIds.indices.groupByReduce[Int,Double](i => factorIds(i), i => res(i), (a,b) => a)
  //   res
  // }

  // // def sampleFactorsConditioned(graph: Rep[FactorGraph[FunctionFactor]], factorIds: Rep[DenseVector[Int]]) = {
  // //   val res = factorIds.map(fid => evaluateFactor(graph, fid))
  // //   factorIds.groupByReduce[Int, Double]
  // //   factorIds.indices.groupByReduce[Int,Double](i => factorIds(i), i => res(i), (a,b) => a)
  // // }

  def learnWeights(graph: Rep[FactorGraph], numIterations: Rep[Int], numSamples: Rep[Int], learningRate: Rep[Double], regularizationConstant: Rep[Double], diminishRate: Rep[Double], times: Rep[DenseVector[Tup2[Int,Long]]]) = {
    val evidenceVariables = DenseVector[Int](graph.variables.length - graph.queryVariables.length, true)
    var i = 0
    var j = 0
    while (j < graph.variables.length) {
      if (graph.variables.apply(j).isEvidence){
        evidenceVariables(i) = graph.variables.apply(j).id
        i += 1
      }
      j += 1
    }
    val evidenceValues = evidenceVariables.map(id => graph.getVariableValue(id))

    println("num_iterations="+numIterations)
    println("num_samples_per_iteration="+numSamples)
    println("learning_rate="+learningRate)
    println("diminish_rate="+diminishRate)
    println("regularization_constant="+regularizationConstant)

      val numCoresPerSocket = getNumCoresPerSocket()
      val numSockets = getNumSockets()
      println(numSockets)
      val numCpp = getNumCpp()
      val numThread = {
        if (numCpp > numCoresPerSocket * numSockets) numCoresPerSocket * numSockets
        else numCpp
      }
      val numThreadLast = (numThread - 1) % numCoresPerSocket + 1
      var iter = 0
      val startTime = time(iter)
      val conditionedEx = (0::graph.factors.length).map(f => evaluateFactor(graph, f))
      var iterLearningRate = learningRate
      while (iter < numIterations / numSockets) {
         val regularization = 1.0/(1.0+regularizationConstant*iterLearningRate)
         println("iteration="+iter+" learning_rate="+iterLearningRate)
         for (tid <- (0::numThread)) {
           val localTid = tid % numCoresPerSocket
           val localNumThread = {
             if (tid / numCoresPerSocket == numSockets - 1) numThreadLast
             else numCoresPerSocket
           }
           val start = (evidenceVariables.length / localNumThread + 1) * localTid
           val temp = start + (evidenceVariables.length / localNumThread + 1)
           val end = if (temp > evidenceVariables.length) evidenceVariables.length else temp
           var id = start
           while (id < end) {
             val variableId = evidenceVariables.apply(id)
             sampleVariable(graph, variableId)
             val iStart = graph.variables.apply(variableId).iStart
             val nFactors = graph.variables.apply(variableId).nFactors
             var index = iStart
             while (index < iStart + nFactors) {
               val factor = graph.variablesToFactors.apply(index)
               if (!graph.weights.apply(factor.weightId).isFixed) {
                 val weightValue = graph.getWeightValue(factor.weightId) + iterLearningRate * (conditionedEx(factor.id) - evaluateFactor(graph, factor.id)) * regularization
                 graph.weightValues.update(factor.weightId, weightValue)
               }
               index += 1
             }
             id += 1
           }
         }
         for (weightId <- (0::graph.weights.length)){
           graph.weightValues.combineAverage(weightId)
         }
        iter += 1
        iterLearningRate = iterLearningRate * diminishRate
      }
       graph.updateVariableValues(evidenceVariables, evidenceValues)
       for (variableId <- evidenceVariables){
         graph.variableValues.combineReplace(variableId)
       }
  }

  def calculateMarginals(graph: Rep[FactorGraph], numSamples: Rep[Int], times: Rep[DenseVector[Tup2[Int,Long]]]) = {
    // val nonEvidenceVariables = graph.queryVariables
    // println("calculating marginals for num_vars="+nonEvidenceVariables.length)
    // val sampleSums = DenseVectorNuma[Double](nonEvidenceVariables.length, true)
    // val sampleSums2 = DenseVectorNuma[Double](nonEvidenceVariables.length, true)
    // var i = 1
    // val startTime = time(i)
    // while (i <= numSamples) {
    //   println("iteration=" + i + "/" + numSamples)
    //   // samples all variables that are not evidence
    //   val start = time()
    //   val z = for (v <- (0::nonEvidenceVariables.length)) {
    //     val sampleResult = sampleVariable(graph, nonEvidenceVariables.apply(v))
    //     val sampleResultSq = sampleResult*sampleResult
    //     sampleSums(v) = sampleSums(v) + sampleResult
    //     sampleSums2(v) = sampleSums2(v) + sampleResultSq
    //   }
    //   for (v <- (0::nonEvidenceVariables.length)) {
    //     graph.variableValues.combineReplace(nonEvidenceVariables.apply(v))
    //   }
    //   val end = time()
    //   i += 1
    // }
    // val iter = i
    // val endTime = time(iter)
    // println("inference sample/sec " + (nonEvidenceVariables.length / ((endTime - startTime) / 1000.0) * numSamples))
    // println("number of query variables " + nonEvidenceVariables.length)
    // println("inference time " + (endTime - startTime))

    // // generate the inference results
    // (0::nonEvidenceVariables.length).map { k =>
    //   val variableId = nonEvidenceVariables.apply(k)
    //   pack((variableId,
    //        sampleSums(k) / numSamples.toDouble,
    //        sqrt(numSamples * sampleSums2(k) - sampleSums(k)*sampleSums(k)) / numSamples,
    //        graph.getVariableValue(variableId)))
    // }
    val nonEvidenceVariables = graph.queryVariables
    println("calculating marginals for num_vars="+nonEvidenceVariables.length)
    val sampleSums = DenseVectorNuma[Double](nonEvidenceVariables.length, true)
    // val sampleSums2 = DenseVectorNuma[Double](nonEvidenceVariables.length, true)
    val numCoresPerSocket = getNumCoresPerSocket()
    val numSockets = getNumSockets()
    val numCpp = getNumCpp()
    val numThread = {
      if (numCpp > numCoresPerSocket * numSockets) numCoresPerSocket * numSockets
      else numCpp
    }
    val numThreadLast = (numThread - 1) % numCoresPerSocket + 1
    var iter = 1
    val startTime = time(iter)
    while (iter <= numSamples / numSockets) {
      println("iteration=" + iter + "/" + numSamples)
      // samples all variables that are not evidence
      for (tid <- (0::numThread)) {
        val localTid = tid % numCoresPerSocket
        val localNumThread = {
          if (tid / numCoresPerSocket == numSockets - 1) numThreadLast
          else numCoresPerSocket
        }
        val start = (nonEvidenceVariables.length / localNumThread + 1) * localTid
        val temp = start + (nonEvidenceVariables.length / localNumThread + 1)
        val end = if (temp > nonEvidenceVariables.length) nonEvidenceVariables.length else temp
        var i = start
        while (i < end) {
          val sampleResult = sampleVariable(graph, nonEvidenceVariables.apply(i))
          sampleSums(i) = sampleSums(i) + sampleResult
          i += 1
        }
      }
      iter += 1
    }
    for (v <- (0::sampleSums.length)) {
      sampleSums.combineAverage(v)
    }
    val endTime = time()
    println("inference sample/sec " + (nonEvidenceVariables.length / ((endTime - startTime) / 1000.0) * numSamples))
    println("number of query variables " + nonEvidenceVariables.length)
    println("inference time " + (endTime - startTime))

    // generate the inference results
    (0::nonEvidenceVariables.length).map { k =>
      val variableId = nonEvidenceVariables.apply(k)
      pack((variableId,
           sampleSums(k) / numSamples.toDouble * numSockets,
           graph.getVariableValue(variableId)))
    }
  }

  def main() = {
    if (args.length < 5) print_usage

    //tic("io")
    val G = readFactorGraph(args(0), args(1), args(2), args(3), args(4), ",")
    //toc("io", G)

    val times1 = DenseVector[Tup2[Int,Long]](0, true)
    val times2 = DenseVector[Tup2[Int,Long]](0, true)

    //tic("learnWeights", G)
    learnWeights(G, 30, 1, 0.01, 0.01, 0.95, times1)
    // //toc("learnWeights", G)
    // writeVector(G.weights.map(w => w.id + "\t" + G.getWeightValue(w.id)), "weights.out")

    //tic("calculateMarginals", G)
    val marginals = calculateMarginals(G, 500, times2)
    //toc("calculateMarginals", marginals)
    writeVector(marginals.map(t => t._1 + "\t" + "\t" + t._2), "marginals.out")

    // val totalNumSamples1 = times1.map(_._1).sum
    // val totalMillis1 = times1.map(_._2).sum
    // println("Learner: samples_per_sec= " + (totalNumSamples1 / (totalMillis1/1000.0)))
    //val totalNumSamples2 = times2.map(_._1).sum
    //val totalMillis2 = times2.map(_._2).sum
    //println("Sampler: samples_per_sec= " + (totalNumSamples2 / (totalMillis2/1000.0)))
    //val totalNumSamples = totalNumSamples1 + totalNumSamples2
    //val totalMillis = totalMillis1 + totalMillis2
    //println("Total: samples_per_sec= " + (totalNumSamples / (totalMillis/1000.0)))
  }
}
