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
    val Tup2 = lookupTpe("Tup2")

    // -- temporary: use java.io.DataInputStream to read binary format, until Delite supports a fixed-length binary reader

    val DataInputStream = tpe("java.io.DataInputStream")
    compiler (IO) ("datainputstream_new", Nil, ("path",MString) :: DataInputStream, effect = simple) implements codegen ($cala, ${
      new java.io.DataInputStream(new java.io.BufferedInputStream(new java.io.FileInputStream($path)))
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
      val start = time()
      val dis = datainputstream_new($path)
      val out = DenseVector[Weight](num_weights, true)
      var i = 0
      while (i < num_weights) {
        val weightId = dis.readLong().toInt
        val isFixed = dis.readBoolean()
        val initialValue = dis.readDouble()
        out(weightId) = Weight(weightId, initialValue, isFixed)
        i += 1
      }
      dis.fclose()
      val z = println("read " + num_weights + " weights")
      val end = time(z) - start
      println("readweights time " + end)
      out.unsafeImmutable
    }

    compiler (IO) ("fg_read_variables", Nil, (("path",MString), ("num_variables",MInt)) :: DenseVector(Tup3(MInt,MDouble,MBoolean))) implements single ${
      val start = time()
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
        out(variableId) = pack((variableId, initialValue, isEvidence))
        i += 1
      }
      dis.fclose()
      val z = println("read " + num_variables + " variables")
      val end = time(z) - start
      println("fg_read_variables time " + end)
      out
    }

    compiler (IO) ("fg_read_factors", Nil, (("path",MString), ("num_factors", MInt)) :: DenseVector(Tup3(MInt,MInt,MInt))) implements single ${
      val start = time()
      val dis = datainputstream_new($path)
      val out = DenseVector[Tup3[Int,Int,Int]](num_factors, true)
      var i = 0
      while (i < num_factors) {
        val factorId = dis.readLong().toInt
        val weightId = dis.readLong().toInt
        val factorFunction = dis.readShort().AsInstanceOf[Int]
        val edgeCount = dis.readLong().toInt
        out(factorId) = pack((factorId, weightId, factorFunction))
        i += 1
      }
      dis.fclose()
      val z = println("read " + num_factors + " factors")
      val end = time(z) - start
      println("fg_read_factors time " + end)
      out.unsafeImmutable
    }

    compiler (IO) ("fg_read_edges", Nil, (("path",MString), ("num_edges", MInt), ("nFactors", MInt), ("nVariables", MInt)) :: Tup2(DenseVector(DenseVector(FactorVariable)), DenseVector(DenseVector(MInt)))) implements single ${
      val start = time()
      val dis = datainputstream_new($path)
      //val out = DenseVector[Tup4[Int,Int,Boolean,Int]](num_edges, true)
      val factorVariablesMap = (0::nFactors) { e => DenseVector[FactorVariable](0, true) }
      val variableFactorsMap = (0::nVariables) { e => DenseVector[Int](0, true) }
      var i = 0
      while (i < num_edges) {
        val variableId = dis.readLong().toInt
        val factorId = dis.readLong().toInt
        val position = dis.readLong().toInt
        val isPositive = dis.readBoolean()
        val equalPredicate = dis.readLong().toInt
        
        factorVariablesMap(factorId) <<= FactorVariable(variableId, isPositive, position)
        variableFactorsMap(variableId) <<= factorId
        //out(i) = pack((variableId, factorId, isPositive, position))
        i += 1
      }
      dis.fclose()
      val z = println("read " + num_edges + " edges")
      val end = time(z) - start
      println("fg_read_edges time " + end)
      //out.unsafeImmutable
      pack((factorVariablesMap.unsafeImmutable, variableFactorsMap.unsafeImmutable))
    }

    compiler(IO) ("calStart", T, (("factorVariablesMap", DenseVector(DenseVector(T))), ("factorStart", DenseVector(MInt)), ("nVariables", DenseVector(MInt))) :: MUnit) implements single ${
      var i = 0
      var count = 0
      val z = while (i < factorStart.length) {
        factorStart(i) = count
        nVariables(i) = factorVariablesMap(i).length
        count = count + nVariables(i)
        i += 1
      }
    }

    direct (IO) ("readFactorGraph", Nil, MethodSignature(List(("metaPath", MString), ("factorsPath", MString), ("variablesPath", MString), ("weightsPath", MString), ("edgesPath", MString), ("delim",MString,"unit(\"\\t\")")), FactorGraph(FunctionFactor))) implements composite ${
      val meta = densevector_fromarray(ForgeFileReader.readLines($metaPath) { line => 
        val tokens = line.trim.fsplit(delim)
        val (num_weights, num_variables, num_factors, num_edges) = (tokens(0).toInt, tokens(1).toInt, tokens(2).toInt, tokens(3).toInt)
        pack((num_weights, num_variables, num_factors, num_edges))
      }, true).apply(0)
      val point_2 = time(meta)
      println("point -2")
      val weights = fg_read_weights($weightsPath, meta._1)//.sortBy(w => w.id)
      val point_1 = time(weights)
      println("point -1")
      val variableRows = fg_read_variables($variablesPath, meta._2)//.sortBy(r => r._1)
      val point0 = time(variableRows)
      println("point 0")
      val factorRows = fg_read_factors($factorsPath, meta._3)//.sortBy(r => r._1)
      val point1 = time(factorRows)
      println("point 1")
      // val factorVariablesMap = (0::factorRows.length) { e => DenseVector[FactorVariable]().mutable } //DenseVector[DenseVector[FactorVariable]](factorRows.length, true)
      // for (r <- factorVariablesMap) { r = DenseVector[FactorVariable]()}
      // val variableFactorsMap = (0::variableRows.length) { e => DenseVector[Int]().mutable } //DenseVector[DenseVector[Int]](variableRows.length, true)
      // for (r <- variableFactorsMap) { r = DenseVector[Int]()}

      val edges = fg_read_edges($edgesPath, meta._4, factorRows.length, variableRows.length)//.sortBy(r => r._2)
      val factorVariablesMap = edges._1
      val variableFactorsMap = edges._2
      val point2 = time(edges)
      println("point 2")
      //val factorVariablesMap = edges.groupBy(r => r._2, r => FactorVariable(r._1, r._3, r._4))
      //val factorVariablesMap = edges.groupBy(r => r._2, r => r._1)
      //val point2a = time(factorVariablesMap)
      //println("point 2a")
      val factorStart = DenseVector[Int](factorRows.length, true)
      val nVariables = DenseVector[Int](factorRows.length, true)
      val z = calStart[FactorVariable](factorVariablesMap, factorStart, nVariables)
      val point3 = time(z)
      println("point 3")
      //val variableFactorsMap = edges.groupBy(r => r._1, r => r._2)
      //val point3a = time(variableFactorsMap)
      //println("point 3a")
      val variableStart = DenseVector[Int](variableRows.length, true)
      val nFactors = DenseVector[Int](variableRows.length, true)
      val zz = calStart[Int](variableFactorsMap, variableStart, nFactors)
      val point4 = time(zz)
      println("point 4")
      val variables = variableRows.indices.map { r => 
        val row = variableRows(r)
        RandomVariable(row._1, 0.0, 1.0, row._2, row._3, nFactors(r), variableStart(r))
      }
      val point5 = time(variables)
      println("point 5")
      val factors = factorRows.indices.map { r =>
        val t = factorRows(r)
        val vars = factorVariablesMap(r).sortBy(x => x.position)
        // val vars = (0::nVariables(r)) { i => 
        //     val x = edges(i + factorStart(r))
        //     FactorVariable(x._1, x._3, x._4)
        // }.sortBy( t => t.position)
        FunctionFactor(t._1, vars, t._2, t._3, nVariables(r), factorStart(r))
      }
      val point6 = time(factors)
      println("point 6")
      val factorsToVariables = build_factor_variables(variables, factors, meta._4)
      val point7 = time(factorsToVariables)
      println("point 7")
      val variablesToFactors = build_variable_factors(variables, factors, variableFactorsMap, meta._4)
      val point8 = time(variablesToFactors)
      println("point 8")
      val variableValues = variables.map(v => v.value).mutable
      val point9 = time(variableValues)
      println("point 9")
      val weightValues = weights.map(w => w.value).mutable
      val point10 = time(weightValues)
      println("point 10")
      println(point_1 - point_2)
      println(point0 - point_1)
      println(point1 - point0)
      println(point2 - point1)
      //println(point2a - point2)
      println(point3 - point2)
      //println(point3a - point3)
      println(point4 - point3)
      println(point5 - point4)
      println(point6 - point5)
      println(point7 - point6)
      println(point8 - point7)
      println(point9 - point8)
      println(point10 - point9)
      FactorGraph(factors, variables, weights, variablesToFactors, factorsToVariables, variableValues, weightValues)
    }
    // -- utility

    /* builds reverse mapping from variables -> factors */
    compiler (IO) ("build_variable_factors", Nil, (("variables", DenseVector(Variable)), ("factors", DenseVector(FunctionFactor)), ("variableFactorsMap", DenseVector(DenseVector(MInt))), ("totalLength", MInt)) :: DenseVector(VariableFactor)) implements composite ${
      val point7a = time()
      val variablesToFactors = DenseVector[VariableFactor](totalLength, true) 
      val point7b = time(variablesToFactors)
      var count = 0
      var i = 0
      val z = variables.map { v =>
        val iStart = v.iStart
        (0::v.nFactors) { j => 
          val factorId = variableFactorsMap(v.id).apply(j)
          val factor = factors(factorId)
          variablesToFactors(iStart + j) = VariableFactor(factorId, factor.funcId, factor.nVariables, factor.iStart, factor.weightId)
        }
      }
      val point7c = time(z)
      println(point7b - point7a)
      println(point7c - point7b)
      variablesToFactors
    }

    compiler (IO) ("build_factor_variables", Nil, (("variables", DenseVector(Variable)), ("factors", DenseVector(FunctionFactor)), ("totalLength", MInt)) :: DenseVector(FactorVariable)) implements composite ${
      val factorsToVariables = DenseVector[FactorVariable](totalLength, true) 
      for (f <- factors.indices){
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
