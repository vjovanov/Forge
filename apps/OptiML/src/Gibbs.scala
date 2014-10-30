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
  def sampleVariable(graph: Rep[FactorGraph], variableId: Rep[Int]) = { //: Rep[Long] = {
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
      //pack(positive * factorWeightValue, negative * factorWeightValue)
      (negative - positive) * factorWeightValue
    }
    
    //val start = time(allSum)
    //val (positiveSum, negativeSum) = (allValues.map(_._1).sum, allValues.map(_._2).sum)
    val newValue = if ((random[Double] * (1.0 + exp(allSum))) <= 1.0) 1.0 else 0.0
    //val newValue = if ((0.5 * (1.0 + exp(allSum))) <= 1.0) 1.0 else 0.0
    //val end = time(newValue)
    val z = graph.updateVariableValue(variableId, newValue)
    newValue
    
    //end - start

    //version 3

    // val variable = graph.variables.apply(variableId)
    // val (variableIStart, variableIEnd) = (variable.iStart, variable.iStart + variable.nFactors)
    // var allSum = 0.0
    // var negativeSum = 0.0
    // val factorsToVariables = graph.factorsToVariables
    // val values = graph.variableValues
    // var i = variableIStart
    // while (i < variableIEnd) {
    //   val factor = graph.variablesToFactors.apply(i)
    //   val positive = factor.evaluate(values, factorsToVariables, variableId, 1.0)
    //   val negative = factor.evaluate(values, factorsToVariables, variableId, 0.0)
    //   val factorWeightValue = graph.getWeightValue(factor.weightId)
    //   allSum += (negative - positive) * factorWeightValue
    //   i += 1
    // }
    // val newValue = if ((random[Double] * (1.0 + exp(allSum))) <= 1.0) 1.0 else 0.0
    // graph.updateVariableValue(variableId, newValue)

    //version 2

    // val variable = graph.variables.apply(variableId)
    // val (variableIStart, variableIEnd) = (variable.iStart, variable.iStart + variable.nFactors)
    // var positiveSum = 0.0
    // var negativeSum = 0.0
    // var i = variableIStart
    // while (i < variableIEnd) {
    //   val factor = graph.variablesToFactors.apply(i)
    //   val (factorIStart, facotrIEnd) = (factor.iStart, factor.iStart + factor.nVariables)
    //   //val positive = DenseVector[Double](factor.nVariables, true)
    //   //val negative = DenseVector[Double](factor.nVariables, true)
    //   var j = factorIStart
    //   var l = 0
    //   while (j < facotrIEnd) {
    //     val v = graph.factorsToVariables.apply(j)
    //     if (v.id == variableId && v.isPositive) {
    //       positive(l) = unit(1.0)
    //       negative(l) = unit(0.0)
    //     }
    //     else if (v.id == variableId && !v.isPositive) {
    //       positive(l) = unit(0.0)
    //       negative(l) = unit(1.0)
    //     }
    //     else {
    //       val value = graph.getVariableValue(v.id, v.isPositive)
    //       positive(l) = value
    //       negative(l) = value
    //     }
    //     l += 1
    //     j += 1
    //   }
    //   val factorWeightValue = graph.getWeightValue(factor.weightId)
    //   positiveSum += factor.evaluate(positive) * factorWeightValue
    //   negativeSum += factor.evaluate(negative) * factorWeightValue
    //   i += 1
    // }
    // val newValue = if ((random[Double] * (1.0 + exp(negativeSum - positiveSum))) <= 1.0) 1.0 else 0.0
    // graph.updateVariableValue(variableId, newValue)

    // version 1

    // // all factors that connect to the variable
    // val variable = graph.variables.apply(variableId)
    // val (variableIStart, variableIEnd) = (variable.iStart, variable.iStart + variable.nFactors)
    // val variableFactors = graph.variablesToFactors.apply(variableIStart::variableIEnd)

    // // TODO: be domain-independent

    // val allValues = variableFactors.map { factor =>
    //   // consider positive and negative cases
    //   val (factorIStart, facotrIEnd) = (factor.iStart, factor.iStart + factor.nVariables)
    //   val vars = graph.factorsToVariables.apply(factorIStart::facotrIEnd)
    //   val cases = vars.map { v =>
    //     if (v.id == variableId && v.isPositive) pack(unit(1.0), unit(0.0))
    //     else if (v.id == variableId && !v.isPositive) pack(unit(0.0), unit(1.0))
    //     else {
    //       val value = graph.getVariableValue(v.id, v.isPositive)
    //       pack(value,value)
    //     }
    //   }

    //   val factorWeightValue = graph.getWeightValue(factor.weightId)
    //   pack(factor.evaluate(cases.map(_._1)) * factorWeightValue,
    //        factor.evaluate(cases.map(_._2)) * factorWeightValue)

    // }

    // val (positiveValues, negativeValues) = (allValues.map(_._1), allValues.map(_._2))

    // val newValue = if ((random[Double] * (1.0 + exp(negativeValues.sum - positiveValues.sum))) <= 1.0) 1.0 else 0.0
    // graph.updateVariableValue(variableId, newValue)
  }

  /* Samples multiple variables and updates the variable values in the graph */
  def sampleVariables(graph: Rep[FactorGraph], variableIds: Rep[DenseVector[Int]], times: Rep[DenseVector[Tup2[Int,Long]]]) = {

    val start = time()
    //val timers = DenseVector[Long](variableIds.length, true)
    //val randomVals = variableIds.map {r => random[Double]}
    val z = for (v <- variableIds) {
      val value = sampleVariable(graph, v)
      //timers(v) = sampleVariable(graph, variableIds(v), randomVals(v))
    }
    
    val end = time(z)
    //println(timers.sum)
    // println(timers.slice(0, variableIds.length/4).sum)
    // println(timers.slice(variableIds.length/4, variableIds.length/2).sum)
    // println(timers.slice(variableIds.length/2, variableIds.length * 3 / 4).sum)
    // println(timers.slice(variableIds.length * 3 / 4, variableIds.length).sum)
    //println(end - start)
    //println(variableIds.length)
    times <<= pack(variableIds.length, end - start)
  }

  def evaluateFactor(graph: Rep[FactorGraph], factorId: Rep[Int]): Rep[Double] = {
    val factor = graph.factors.apply(factorId)
    val values = graph.variableValues
    val factorsToVariables = graph.factorsToVariables
    val variableId = factorsToVariables(factor.iStart).id
    factor.evaluate(values, factorsToVariables, variableId, values(variableId))
    // val factorVariableValues = factor.vars.map(fv => graph.getVariableValue(fv.id, fv.isPositive))
    // factor.evaluate(factorVariableValues)
  }

  // computes the marginal probability that each factor is true
  def sampleFactors(graph: Rep[FactorGraph], variableIds: Rep[DenseVector[Int]], factorIds: Rep[DenseVector[Int]], numSamples: Rep[Int], times: Rep[DenseVector[Tup2[Int,Long]]]) = {
    var i = 0
    val acc = DenseVector[Double](factorIds.length, true)
    while (i < numSamples) {
      // sample all variables and update their values
      sampleVariables(graph, variableIds, times)
      acc += factorIds.map(fid => evaluateFactor(graph, fid))
      i += 1
    }
    val res = acc / numSamples
    //factorIds.indices.groupByReduce[Int,Double](i => factorIds(i), i => res(i), (a,b) => a)
    res
  }

  // def sampleFactorsConditioned(graph: Rep[FactorGraph[FunctionFactor]], factorIds: Rep[DenseVector[Int]]) = {
  //   val res = factorIds.map(fid => evaluateFactor(graph, fid))
  //   factorIds.groupByReduce[Int, Double]
  //   factorIds.indices.groupByReduce[Int,Double](i => factorIds(i), i => res(i), (a,b) => a)
  // }

  // TIME BREAKDOWN: sequential total is ~53s
  // sampling factors takes ~.5s per iteration --> 50s overall
  // sampleVariables is ~.17s per call * 2x per iteration --> 34s overall
  def learnWeights(graph: Rep[FactorGraph], numIterations: Rep[Int], numSamples: Rep[Int], learningRate: Rep[Double], regularizationConstant: Rep[Double], diminishRate: Rep[Double], times: Rep[DenseVector[Tup2[Int,Long]]]): Rep[Unit] = {
    tic("initLearnWeights")
    //val allVariables = graph.variables.map(_.id)
    val queryVariables = graph.variables.filter(!_.isEvidence).map(_.id)
    val evidenceVariables = graph.variables.filter(_.isEvidence).map(_.id)
    val evidenceValues = evidenceVariables.map(id => graph.getVariableValue(id))

    // we only learn weights for factors that are connected to evidence
    val queryFactorIds = evidenceVariables.flatMap { vid =>
      val iStart = graph.variables.apply(vid).iStart
      val queryFactorindices = (0::graph.variables.apply(vid).nFactors) {e => e + iStart}
      queryFactorindices.map(i => graph.variablesToFactors.apply(i).id)
    }.distinct
    val factorWeightIds = queryFactorIds.map(fid => graph.factors.apply(fid).weightId).distinct
    //val queryWeightIds = factorWeightIds.filter(wid => !graph.weights.apply(wid).isFixed)
    val queryWeightIds = queryFactorIds.map(r => graph.factors.apply(r).weightId).filter(wid => !graph.weights.apply(wid).isFixed)
    //val weightFactorIdsMap = queryFactorIds.map(fid => graph.factors.apply(fid)).groupBy(f => f.weightId, f => f.id)
    //toc("initLearnWeights", allVariables, queryVariables, evidenceValues, queryWeightIds, weightFactorIdsMap)
    toc("initLearnWeights", queryVariables, evidenceValues, queryWeightIds)

    println("num_iterations="+numIterations)
    println("num_samples_per_iteration="+numSamples)
    println("learning_rate="+learningRate)
    println("diminish_rate="+diminishRate)
    println("regularization_constant="+regularizationConstant)
    println("num_factors="+graph.factors.length+" num_query_factors="+queryFactorIds.length)
    println("num_weights="+graph.weights.length+" num_query_weights="+queryWeightIds.length)
    println("num_query_variables="+queryVariables.length+" num_evidence_variables="+evidenceVariables.length)

    if (queryWeightIds.length == 0) {
      println("no query weights, nothing to learn!")
    }
    else {
      //val conditionedEx = sampleFactorsConditioned(graph, queryFactorIds)
      val conditionedEx = queryFactorIds.map(fid => evaluateFactor(graph, fid))
      untilconverged(0, minIter = numIterations, maxIter = numIterations) { i =>
        val iterLearningRate = pow(diminishRate, i) * learningRate

        println("iteration="+i+" learning_rate="+iterLearningRate)

        //tic("sampleFactors")
        // compute the expectation for all factors sampling only query variables
        // compute the expectation for all factors sampling all variables
        val unconditionedEx = sampleFactors(graph, evidenceVariables, queryFactorIds, numSamples, times)
        //toc("sampleFactors", conditionedEx, unconditionedEx)

        // compute new weights
        val weightUpdates = queryFactorIds.indices.map { i =>
          val weightId = graph.factors.apply(queryFactorIds(i)).weightId
          val weightChange = conditionedEx(i) - unconditionedEx(i)
          pack(weightId, weightChange)
        }.groupByReduce[Int, Double](r => r._1, r => r._2, (a, b) => a + b)
        val weightChanges = queryWeightIds.map {r =>
          val weightChange = weightUpdates(r)
          val currentWeight = graph.getWeightValue(r)
          val newWeight = currentWeight + (weightChange * iterLearningRate) * (1.0/(1.0+regularizationConstant*iterLearningRate))
          graph.updateWeightValue(r, newWeight)
          weightChange
        }

        // val weightUpdates = queryWeightIds.map { weightId =>
        //   val factors = weightFactorIdsMap(weightId)
        //   val currentWeight = graph.getWeightValue(weightId)
        //   def withDefaultZero(m: Rep[ForgeHashMap[Int,Double]], key: Rep[Int]) = if (m.contains(key)) m(key) else 0.0
        //   val weightChange = factors.map(id => (withDefaultZero(conditionedEx,id) - withDefaultZero(unconditionedEx,id))).sum
        //   val newWeight = currentWeight + (weightChange * iterLearningRate) * (1.0/(1.0+regularizationConstant*iterLearningRate))
        //   pack(weightChange, newWeight)
        // }

        // graph.updateWeightValues(queryWeightIds, weightUpdates.map(_._2))
        // val weightChanges = weightUpdates.map(t => t._1)

        // calculate the L2 norm of the weight changes and the maximum gradient
        val gradientNorm = sqrt(sum(square(weightChanges)))
        val maxGradient = max(abs(weightChanges))
        println("gradient_norm="+gradientNorm+" max_gradient="+maxGradient)

        // reset the evidence variables to their evidence values (we changed their values by sampling them above)

        i + 1
      }
      graph.updateVariableValues(evidenceVariables, evidenceValues)
      ()
    }

  }

  def calculateMarginals(graph: Rep[FactorGraph], numSamples: Rep[Int], variables: Rep[DenseVector[RandomVariable]], times: Rep[DenseVector[Tup2[Int,Long]]]) = {
    println("calculating marginals for num_vars="+variables.length)

    val nonEvidenceVariables = variables.filter(!_.isEvidence).map(_.id)
    // Sums of all samples values to calculate the expectation
    val sampleSums = DenseVector[Double](nonEvidenceVariables.length, true)
    // Squared sample sums to calculate running standard deviation
    val sampleSums2 = DenseVector[Double](nonEvidenceVariables.length, true)
    val numThreads = getNumThreads()
    println(numThreads)
    val range = nonEvidenceVariables.length / numThreads + 1
    val startTime = time()
    val z = for (thread <- (0::numThreads)) {
      val localStartTime = time()
      var iter = 0
      var start = thread * range
      while (iter < numSamples){
        val end = if (start + range > nonEvidenceVariables.length) nonEvidenceVariables.length else (start + range)
        var v = start 
        while (v < end){
          val sampleResult = sampleVariable(graph, nonEvidenceVariables(v))
          val sampleResultSq = sampleResult*sampleResult
          sampleSums(v) = sampleSums(v) + sampleResult
          sampleSums2(v) = sampleSums2(v) + sampleResultSq
          v += 1
        }
        start = if (end == nonEvidenceVariables.length) 0 else end
        iter += 1
      }
      val localEndTIme = time()
      println("thread " + thread + " use " + (localEndTIme - localStartTime))
    }
    val endTime = time(z)
    println("inference sample/sec " + (nonEvidenceVariables.length / ((endTime - startTime) / 1000.0) * numSamples))
    println("number of query variables " + nonEvidenceVariables.length)
    println("inference time " + (endTime - startTime))
    //times <<= pack(numSamples * nonEvidenceVariables.length, endTime - startTime)

    // generate the inference results
    nonEvidenceVariables.indices.map { k =>
      val variableId = nonEvidenceVariables(k)
      pack((variableId,
           sampleSums(k) / numSamples.toDouble,
           sqrt(numSamples * sampleSums2(k) - sampleSums(k)*sampleSums(k)) / numSamples,
           graph.getVariableValue(variableId)))
    }
  }

  def main() = {
    if (args.length < 5) print_usage

    tic("io")
    val G = readFactorGraph(args(0), args(1), args(2), args(3), args(4), ",")
    toc("io", G)

    val times1 = DenseVector[Tup2[Int,Long]](0, true)
    val times2 = DenseVector[Tup2[Int,Long]](0, true)

    tic("learnWeights", G)
    learnWeights(G, 300, 1, 0.01, 0.1, 0.95, times1)
    toc("learnWeights", G)
    writeVector(G.weights.map(w => w.id + "\t" + G.getWeightValue(w.id)), "weights.out")

    tic("calculateMarginals", G)
    val marginals = calculateMarginals(G, 500, G.variables, times2)
    toc("calculateMarginals", marginals)
    writeVector(marginals.map(t => t._1 + "\t" + t._4.toInt + "\t" + t._2), "marginals.out")

    val totalNumSamples1 = times1.map(_._1).sum
    val totalMillis1 = times1.map(_._2).sum
    println("Learner: samples_per_sec= " + (totalNumSamples1 / (totalMillis1/1000.0)))
    //val totalNumSamples2 = times2.map(_._1).sum
    //val totalMillis2 = times2.map(_._2).sum
    //println("Sampler: samples_per_sec= " + (totalNumSamples2 / (totalMillis2/1000.0)))
    //val totalNumSamples = totalNumSamples1 + totalNumSamples2
    //val totalMillis = totalMillis1 + totalMillis2
    //println("Total: samples_per_sec= " + (totalNumSamples / (totalMillis/1000.0)))
  }
}