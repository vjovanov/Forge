package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}
import factor._

trait MLIOOps {
  this: OptiMLDSL =>

  lazy val IO = grp("MLio")

  def importMLIOOps() {
    importFactorIOOps()
    importARFFOps()
  }

  def importFactorIOOps() {  
    val T = tpePar("T")
    val DenseVector = lookupTpe("DenseVector")
    val FactorGraph = lookupTpe("FactorGraph")
    val Weight = lookupTpe("Weight")
    val FactorVariable = lookupTpe("FactorVariable")
    val Variable = lookupTpe("RandomVariable")
    val FunctionFactor = lookupTpe("FunctionFactor")
    val SHashMap = lookupTpe("scala.collection.mutable.HashMap")
    val SArray = tpe("scala.Array", T)
    val Any = lookupTpe("Any")


    compiler (IO) ("readWeights", Nil, ("path",MString) :: SArray(SArray(Any))) implements codegen ($cala, ${
      val dis = new java.io.DataInputStream(new java.io.FileInputStream($path))
      val weights = new scala.collection.mutable.ArrayBuffer[Int]()
      val fixes = new scala.collection.mutable.ArrayBuffer[Boolean]()
      val inivalues = new scala.collection.mutable.ArrayBuffer[Double]()
      while (dis.available()>0) {
        val weightId : Int = dis.readLong().toInt
        val isFixed : Boolean = dis.readBoolean()
        val initialValue : Double= dis.readDouble()
        weights += weightId
        fixes += isFixed
        inivalues += initialValue
      }
      dis.close()
      val out : Array[Array[Any]] = Array(weights.toArray, inivalues.toArray, fixes.toArray)
      out
    })

    compiler (IO) ("readVariables", Nil, ("path",MString) :: SArray(SArray(Any))) implements codegen ($cala, ${
      val dis = new java.io.DataInputStream(new java.io.FileInputStream($path))
      val variables = new scala.collection.mutable.ArrayBuffer[Int]()
      val evidences = new scala.collection.mutable.ArrayBuffer[Boolean]()
      val inivalues = new scala.collection.mutable.ArrayBuffer[Double]()
      val queries = new scala.collection.mutable.ArrayBuffer[Boolean]()
      while (dis.available()>0) {
        val variableId : Int = dis.readLong().toInt
        val isEvidence : Boolean = dis.readBoolean()
        val initialValue : Double = dis.readDouble()
        val dataType : Short = dis.readShort()
        val edgeCount : Int = dis.readLong().toInt
        val cardinality : Int = dis.readLong().toInt
        val isQuery : Boolean = !isEvidence
        variables += variableId
        evidences += isEvidence
        inivalues += initialValue
        queries += isQuery
      }
      dis.close()
      val out : Array[Array[Any]] = Array(variables.toArray, inivalues.toArray, evidences.toArray, queries.toArray)
      out
    })

    compiler (IO) ("readEdges", Nil, ("path",MString) :: SArray(SArray(Any))) implements codegen ($cala, ${
      val dis = new java.io.DataInputStream(new java.io.FileInputStream($path))
      val variables = new scala.collection.mutable.ArrayBuffer[Int]()
      val factors = new scala.collection.mutable.ArrayBuffer[Int]()
      val positives = new scala.collection.mutable.ArrayBuffer[Boolean]()
      val positions = new scala.collection.mutable.ArrayBuffer[Int]()

      while (dis.available()>0) {
        val variableId : Int = dis.readLong().toInt
        val factorId : Int = dis.readLong().toInt
        val position : Int = dis.readLong().toInt
        val isPositive : Boolean = dis.readBoolean()
        val equalPredicate : Int = dis.readLong().toInt
        variables += variableId
        factors += factorId
        positives += isPositive
        positions += position
      }
      dis.close()
      val out : Array[Array[Any]] = Array(variables.toArray, factors.toArray, positives.toArray, positions.toArray)
      out
    })

    // -- input
    compiler (IO) ("readFactors", Nil, ("path",MString) :: SArray(SArray(Any))) implements codegen ($cala, ${
      val dis = new java.io.DataInputStream(new java.io.FileInputStream($path))
      val factors = new scala.collection.mutable.ArrayBuffer[Int]()
      val weights = new scala.collection.mutable.ArrayBuffer[Int]()
      val funcs = new scala.collection.mutable.ArrayBuffer[Int]()
      while (dis.available()>0) {
        val factorId : Int = dis.readLong().toInt
        val weightId : Int = dis.readLong().toInt
        val factorFunction : Int = dis.readShort()
        val edgeCount : Int = dis.readLong().toInt
        factors += factorId
        weights += weightId
        funcs += factorFunction
      }
      dis.close()
      val out : Array[Array[Any]] = Array(factors.toArray, weights.toArray, funcs.toArray)
      out
    })

    direct (IO) ("readFactorGraphNew", Nil, MethodSignature(List(("factorsPath", MString), ("variablesPath", MString), ("weightsPath", MString), ("edgesPath", MString), ("delim",MString,"\"\\t\"")), FactorGraph(FunctionFactor))) implements composite ${
      val weight = readWeights($weightsPath)
      val a = farray_from_sarray(weight(0))
      val b = farray_from_sarray(weight(1))
      val c = farray_from_sarray(weight(2))
      val weight_tuple = (0::array_length(a)) { i => Weight(a(i).AsInstanceOf[Int], b(i).AsInstanceOf[Double], c(i).AsInstanceOf[Boolean]) }
      val weights = weight_tuple.sortBy(w => w.id)
      val variable = readVariables($variablesPath)
      val d = farray_from_sarray(variable(0))
      val e = farray_from_sarray(variable(1))
      val f = farray_from_sarray(variable(2))
      val g = farray_from_sarray(variable(3))
      val variable_tuple = (0::array_length(d)) { i => pack((d(i).AsInstanceOf[Int], e(i).AsInstanceOf[Double], f(i).AsInstanceOf[Boolean], g(i).AsInstanceOf[Boolean])) }
      val variables = variable_tuple.map(r => RandomVariable(r._1, DenseVector(0.0, 1.0), r._2, r._3, r._4)).distinct.sortBy(r => r.id)
      val edge = readEdges($edgesPath)
      val l = farray_from_sarray(edge(0))
      val m = farray_from_sarray(edge(1))
      val n = farray_from_sarray(edge(2))
      val s = farray_from_sarray(edge(3))
      val edges = (0::array_length(l)) { i => pack((l(i).AsInstanceOf[Int], m(i).AsInstanceOf[Int], n(i).AsInstanceOf[Boolean], s(i).AsInstanceOf[Int]))}
      val factorVariablesMap = edges.groupBy(r => r._2, r => FactorVariable(r._1, r._3, DenseVector(0.0, 1.0), r._4))
      val factor = readFactors($factorsPath)
      val o = farray_from_sarray(factor(0))
      val p = farray_from_sarray(factor(1))
      val q = farray_from_sarray(factor(2))
      val factorRows = (0::array_length(o)) { i => pack((o(i).AsInstanceOf[Int], p(i).AsInstanceOf[Int], q(i).AsInstanceOf[Int])) }
      val allFactors = factorRows.map { t =>
        val vars = if (factorVariablesMap.contains(t._1)) factorVariablesMap(t._1).sortBy(r => r.position) else DenseVector[FactorVariable]()
        FunctionFactor(t._1, vars, t._2, t._3)
      }
      val factors = allFactors.filter(f => f.vars.length > 0).sortBy(f => f.id)

      val variablesToFactors = build_variable_factors(variables, factors)
      val variableValues = variables.map(v => v.value).mutable
      val weightValues = weights.map(w => w.value).mutable

      FactorGraph(factors, variables, weights, variablesToFactors, variableValues, weightValues)      
    }

    // currently we only support DeepDive-style factor graphs
    direct (IO) ("readFactorGraph", Nil, MethodSignature(List(("factorsPath", MString), ("variablesPath", MString), ("weightsPath", MString), ("delim",MString,"\"\\t\"")), FactorGraph(FunctionFactor))) implements composite ${
      val weights = densevector_fromarray(ForgeFileReader.readLines($weightsPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (id, initialValue, isFixed) = (tokens(0).toInt, tokens(1).toDouble, tokens(2).toBoolean)
        Weight(id, initialValue, isFixed)
      }, true).sortBy(w => w.id)
      
      val variableRows = ForgeFileReader.readLines($variablesPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (variableId, factorId, position, isPositive, dataType, initialValue, isEvidence, isQuery) = (tokens(0).toInt, tokens(1).toInt, tokens(2).toInt, tokens(3).toBoolean, tokens(4), tokens(5).toDouble, tokens(6).toBoolean, tokens(7).toBoolean)
        pack((variableId, factorId, position, isPositive, dataType, initialValue, isEvidence, isQuery))
      }

      // currently only supporting boolean vars
      val variables = densevector_fromarray(variableRows, true).map(r => RandomVariable(r._1, DenseVector(0.0, 1.0), r._6, r._7, r._8)).distinct.sortBy(r => r.id)
      
      val factorVariablesMap = densevector_fromarray(variableRows, true).groupBy(r => r._2, r => FactorVariable(r._1, r._4, DenseVector(0.0, 1.0), r._3))

      // reading factors breaks in parallel if we try to look up the hashmap inside the file reading loop, for some reason
      val factorRows = ForgeFileReader.readLines($factorsPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (factorId, weightId, func) = (tokens(0).toInt, tokens(1).toInt, tokens(2).toInt)
        // val vars = if (factorVariablesMap.contains(factorId)) factorVariablesMap(factorId) else DenseVector[FactorVariable]()
        // FunctionFactor(factorId, vars, weightId, func)
        pack((factorId, weightId, func))
      }
      val allFactors = factorRows.map { t =>
        val vars = if (factorVariablesMap.contains(t._1)) factorVariablesMap(t._1).sortBy(r => r.position) else DenseVector[FactorVariable]()
        FunctionFactor(t._1, vars, t._2, t._3)
      }
      val factors = densevector_fromarray(allFactors, true).filter(f => f.vars.length > 0).sortBy(f => f.id)
      
      val variablesToFactors = build_variable_factors(variables, factors)
      val variableValues = variables.map(v => v.value).mutable
      val weightValues = weights.map(w => w.value).mutable

      FactorGraph(factors, variables, weights, variablesToFactors, variableValues, weightValues)      
    }


    // -- output



    // -- utility

    /* builds reverse mapping from variables -> factors */
    compiler (IO) ("build_variable_factors", Nil, (("variables", DenseVector(Variable)), ("factors", DenseVector(FunctionFactor))) :: DenseVector(DenseVector(MInt))) implements single ${
      val variablesToFactors = DenseVector[DenseVector[Int]](variables.length, true) 

      for (i <- 0 until variablesToFactors.length) {
        variablesToFactors(i) = DenseVector[Int]()
      }

      val factorIds = factors.indices
      for (i <- 0 until factorIds.length) {
        val f = factors(factorIds(i))
        for (j <- 0 until f.vars.length) {
          val vId = f.vars.apply(j).id
          val curVec = variablesToFactors(vId)
          variablesToFactors.update(vId, curVec << factorIds(i))
        }
      }
   
      variablesToFactors.map(v => v.distinct)      
    }

    ()
  
  }

  def importARFFOps() {
  	val Row = tpePar("Row")
  	val DenseVector = lookupTpe("DenseVector")

  	direct (IO) ("readARFF", Row, MethodSignature(List(("path",MString),("schemaBldr",DenseVector(MString) ==> Row)), DenseVector(Row)), effect = simple) implements composite ${
  	  // INVESTIGATE: lines and start computations, and body and map computations, fuse unsafely without .mutable
  	  // the latter appears to be the bug of not having the filter condition properly guard the subsequent computation
  	  val lines = densevector_fromarray(ForgeFileReader.readLines($path){ line => line.trim }, true).mutable
  	  
  	  // skip past the header to the data section
      // since we are using schemaBldr, we don't care about the attribute types
  	  val start = lines find { _ == "@DATA" }
  	  if (start.length < 1) fatal("could not find @DATA tag in ARFF file: " + $path)
  	  val body = lines.drop(start(0)+1).filter(!_.startsWith("%")).mutable
  	  body map { s => schemaBldr(densevector_fromarray(s.fsplit(","), true)) }
    }
  }
}
