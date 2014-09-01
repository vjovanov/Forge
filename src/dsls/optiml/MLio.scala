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
    val VariableFactor = lookupTpe("VariableFactor")
    val Variable = lookupTpe("RandomVariable")
    val FunctionFactor = lookupTpe("FunctionFactor")
    val Tup3 = lookupTpe("Tup3")
    val Tup4 = lookupTpe("Tup4")

    // -- temporary: use java.io.DataInputStream to read binary format, until Delite supports a fixed-length binary reader

    val DataInputStream = tpe("java.io.DataInputStream")
    compiler (IO) ("datainputstream_new", Nil, ("path",MString) :: DataInputStream, effect = simple) implements codegen ($cala, ${
      new java.io.DataInputStream(new java.io.FileInputStream($path))
    })

    infix (IO) ("available", Nil, DataInputStream :: MInt, effect = simple) implements codegen ($cala, ${
      $0.available()
    })

    // infix close clashes with LMS IOOps
    infix (IO) ("fclose", Nil, DataInputStream :: MUnit, effect = simple) implements codegen ($cala, ${
      $0.close()
    })

    infix (IO) ("readShort", Nil, DataInputStream :: MShort, effect = simple) implements codegen ($cala, ${
      $0.readShort()
    })

    infix (IO) ("readInt", Nil, DataInputStream :: MInt, effect = simple) implements codegen ($cala, ${
      $0.readInt()
    })

    infix (IO) ("readLong", Nil, DataInputStream :: MLong, effect = simple) implements codegen ($cala, ${
      $0.readLong()
    })

    infix (IO) ("readDouble", Nil, DataInputStream :: MDouble, effect = simple) implements codegen ($cala, ${
      $0.readDouble()
    })

    infix (IO) ("readBoolean", Nil, DataInputStream :: MBoolean, effect = simple) implements codegen ($cala, ${
      $0.readBoolean()
    })

    // -- input

    compiler (IO) ("fg_read_weights", Nil, (("path",MString), ("num_weights",MInt)) :: DenseVector(Weight)) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[Weight](num_weights, true)
      var i = 0
      while (i < num_weights) {
        val weightId = dis.readLong().toInt
        val isFixed = dis.readBoolean()
        val initialValue = dis.readDouble()
        if (i < 10) {
          println(out(i).id + " , " + out(i).value + " , " + out(i).isFixed)
        }
        out(i) = Weight(weightId, initialValue, isFixed)
        if (i < 10) {
          println(out(i).id + " , " + out(i).value + " , " + out(i).isFixed)
        }
        i += 1
      }
      dis.fclose()
      println("read " + num_weights + " weights")
      out.unsafeImmutable
    }

    compiler (IO) ("fg_read_variables", Nil, (("path",MString), ("num_variables",MInt)) :: DenseVector(Tup3(MInt,MDouble,MBoolean))) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[Tup3[Int, Double, Boolean]](num_variables, true)
      var i = 0
      while (i < num_variables) {
        val variableId = dis.readLong().toInt
        val isEvidence = dis.readBoolean()
        val initialValue = dis.readDouble()
        val dataType = dis.readShort()
        val edgeCount = dis.readLong().toInt
        val cardinality = dis.readLong().toInt
        val isQuery = !isEvidence
        out(i) = pack((variableId, initialValue, isEvidence))
        i += 1
      }
      dis.fclose()
      println("read " + num_variables + " variables")
      out
    }

    compiler (IO) ("fg_read_edges", Nil, (("path",MString), ("num_edges", MInt)) :: DenseVector(Tup4(MInt,MInt,MBoolean,MInt))) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[Tup4[Int,Int,Boolean,Int]](num_edges, true)
      var i = 0
      while (i < num_edges) {
        val variableId = dis.readLong().toInt
        val factorId = dis.readLong().toInt
        val position = dis.readLong().toInt
        val isPositive = dis.readBoolean()
        val equalPredicate = dis.readLong().toInt
        
        out(i) = pack((variableId, factorId, isPositive, position))
        i += 1
      }
      dis.fclose()
      println("read " + num_edges + " edges")
      out.unsafeImmutable
    }

    compiler (IO) ("fg_read_factors", Nil, (("path",MString), ("num_factors", MInt)) :: DenseVector(Tup3(MInt,MInt,MInt))) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[Tup3[Int,Int,Int]](num_factors, true)
      var i = 0
      while (i < num_factors) {
        val factorId = dis.readLong().toInt
        val weightId = dis.readLong().toInt
        val factorFunction = dis.readShort().AsInstanceOf[Int]
        val edgeCount = dis.readLong().toInt
        out(i) = pack((factorId, weightId, factorFunction))
        i += 1
      }
      dis.fclose()
      println("read " + num_factors + " factors")
      out.unsafeImmutable
    }

    direct (IO) ("readFactorGraph", Nil, MethodSignature(List(("metaPath", MString), ("factorsPath", MString), ("variablesPath", MString), ("weightsPath", MString), ("edgesPath", MString), ("delim",MString,"unit(\"\\t\")")), FactorGraph(FunctionFactor))) implements composite ${
      val meta = densevector_fromarray(ForgeFileReader.readLines($metaPath) { line => 
        val tokens = line.trim.fsplit(delim)
        val (num_weights, num_variables, num_factors, num_edges) = (tokens(0).toInt, tokens(1).toInt, tokens(2).toInt, tokens(3).toInt)
        pack((num_weights, num_variables, num_factors, num_edges))
      }, true).apply(0)
      val weights = fg_read_weights($weightsPath, meta._1).sortBy(w => w.id)      
      val variableRows = fg_read_variables($variablesPath, meta._2).sortBy(r => r._1)
      println("point 0")
      val factorRows = fg_read_factors($factorsPath, meta._3).sortBy(r => r._1)
      println("point 1")
      val edges = fg_read_edges($edgesPath, meta._4)
      println("point 2")
      val factorVariablesMap = edges.groupBy(r => r._2, r => FactorVariable(r._1, r._3, r._4))
      val factorStart = DenseVector[Int](factorRows.length, true)
      var i = 0
      var count = 0
      while (i < factorRows.length) {
        factorStart(i) = count
        count = count + factorVariablesMap(factorRows(i)._1).length
        i += 1
      }
      println("point 3")
      val variableFactorsMap = edges.groupBy(r => r._1, r => r._2)
      val variableStart = DenseVector[Int](variableRows.length, true)
      val nFactors = DenseVector[Int](variableRows.length, true)
      count = 0
      i = 0
      while (i < variableRows.length){
        variableStart(i) = count
        nFactors(i) = variableFactorsMap(variableRows(i)._1).length
        count = count + variableFactorsMap(variableRows(i)._1).length
        i += 1
      }
      println("point 4")
      val variables = variableRows.indices.map { r => 
        val row = variableRows(r)
        RandomVariable(row._1, 0.0, 1.0, row._2, row._3, nFactors(r), variableStart(r))
      }
      println("point 5")
      val factors = factorRows.indices.map { r =>
        val t = factorRows(r)
        val vars = if (factorVariablesMap.contains(t._1)) factorVariablesMap(t._1).sortBy(x => x.position) else DenseVector[FactorVariable]()
        val nVariables = vars.length
        if (r < 50){
          println(nVariables)
        }
        FunctionFactor(t._1, vars, t._2, t._3, nVariables, factorStart(r))
      }
      println("point 6")
      nFactors(0::50).pprint
      val factorsToVariables = build_factor_variables(variables, factors)
      println("point 7")
      val variablesToFactors = build_variable_factors(variables, factors, variableFactorsMap)
      println("point 8")
      val variableValues = variables.map(v => v.value).mutable
      println("point 9")
      val weightValues = weights.map(w => w.value).mutable
      println("point 10")
      FactorGraph(factors, variables, weights, variablesToFactors, factorsToVariables, variableValues, weightValues)
    }

    /*
    // old DeepDive factor graph formats
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
    */

    // -- output



    // -- utility

    /* builds reverse mapping from variables -> factors */
    compiler (IO) ("build_variable_factors", Nil, (("variables", DenseVector(Variable)), ("factors", DenseVector(FunctionFactor)), ("variableFactorsMap", MHashMap(MInt, DenseVector(MInt)))) :: DenseVector(VariableFactor)) implements composite ${
      // val tempVariablesToFactors = DenseVector[DenseVector[Int]](variables.length, true) 
      // println(variables.map(v => v.nFactors).sum)
      // println(variables(0).nFactors)
      // println(variables(1).nFactors)
      // println(variables(2).nFactors)
      // val variablesToFactors = DenseVector[VariableFactor](variables.map(v => v.nFactors).sum, true) 
      // for (i <- 0 until tempVariablesToFactors.length) {
      //   tempVariablesToFactors(i) = DenseVector[Int]()
      // }
      // val factorIds = factors.indices
      // for (i <- 0 until factorIds.length) {
      //   val f = factors(factorIds(i))
      //   for (j <- 0 until f.vars.length) {
      //     val vId = f.vars.apply(j).id
      //     val curVec = tempVariablesToFactors(vId)
      //     tempVariablesToFactors.update(vId, curVec << factorIds(i))
      //   }
      // }
      var count = 0
      var i = 0
      while (i < variables.length){
        val singleVariablefactorIds = variableFactorsMap(i)
        for (j <- singleVariablefactorIds.indices){
          variablesToFactors(count + j) = VariableFactor(singleVariablefactorIds(j), factors(singleVariablefactorIds(j)).funcId, factors(singleVariablefactorIds(j)).nVariables, factors(singleVariablefactorIds(j)).iStart, factors(singleVariablefactorIds(j)).weightId)
        }
        count = count + variableFactorsMap(i).length
        i += 1
      }
      // for (v <- temp.indices){
      //   val row = temp.apply(v)
      //   for (f <- row.indices){
      //     val id = row.apply(f)
      //     val position = variables(v).iStart + f
      //     variablesToFactors(position) = VariableFactor(id, factors(id).funcId, factors(id).nVariables, factors(id).iStart)
      //   }
      // }

      variablesToFactors
    }




    compiler (IO) ("build_factor_variables", Nil, (("variables", DenseVector(Variable)), ("factors", DenseVector(FunctionFactor))) :: DenseVector(FactorVariable)) implements composite ${
      val factorsToVariables = DenseVector[FactorVariable](factors.map(v => v.nVariables).sum, true) 
      for (f <- factors.indices){
        if (factors(f).id != f) {
          println("error! Bad factor graph!")
        }
        for (v <- factors(f).vars.indices){
          val position = factors(f).iStart + v
          factorsToVariables(position) = factors(f).vars.apply(v)
        }
      }
      factorsToVariables
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
