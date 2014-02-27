package ppl.dsl.forge
package dsls.optiml
package factor

import core.{ForgeApplication,ForgeApplicationRunner}

trait FactorGraphOps {
  this: OptiMLDSL =>

  def importAllFactorGraphOps() {
    importAllFactorOps()
    importVariableOps()
    importWeightOps()
    importFactorGraphOps()
  }

  def importVariableOps() {
    val DenseVector = lookupTpe("DenseVector")
    val Variable = tpe("RandomVariable")
    data(Variable, ("_id", MInt), ("_domain", DenseVector(MDouble)), ("_value", MDouble), ("_isEvidence", MBoolean), ("_isQuery", MBoolean))

    static (Variable) ("apply", Nil, (("id", MInt), ("domain", DenseVector(MDouble)), ("value", MDouble), ("isEvidence", MBoolean), ("isQuery", MBoolean)) :: Variable) implements
      allocates(Variable, ${$0}, ${$1}, ${$2}, ${$3}, ${$4})

    val VariableOps = withTpe(Variable)
    VariableOps {
      infix ("id") (Nil :: MInt) implements getter(0, "_id")
      infix ("domain") (Nil :: DenseVector(MDouble)) implements getter(0, "_domain")
      infix ("value") (Nil :: MDouble) implements getter(0, "_value")
      infix ("isEvidence") (Nil :: MBoolean) implements getter(0, "_isEvidence")
      infix ("isQuery") (Nil :: MBoolean) implements getter(0, "_isQuery")
    }
  }

  def importWeightOps() {
    val Weight = tpe("Weight")
    data(Weight, ("_id", MInt), ("_value", MDouble), ("_isFixed", MBoolean))

    static (Weight) ("apply", Nil, (("id", MInt), ("value", MDouble), ("isFixed", MBoolean)) :: Weight) implements allocates(Weight, ${$0}, ${$1}, ${$2})

    val WeightOps = withTpe(Weight)
    WeightOps {
      infix ("id") (Nil :: MInt) implements getter(0, "_id")
      infix ("value") (Nil :: MDouble) implements getter(0, "_value")
      infix ("isFixed") (Nil :: MBoolean) implements getter(0, "_isFixed")
    }
  }

  def importFactorGraphOps() {
    val DenseVector = lookupTpe("DenseVector")
    val SHashMap = lookupTpe("scala.collection.mutable.HashMap")
    val CHashMap = lookupTpe("java.util.concurrent.ConcurrentHashMap")
    val Variable = lookupTpe("RandomVariable")
    val Weight = lookupTpe("Weight")
    val F = tpePar("F") withBound TFactor
    val FactorGraph = tpe("FactorGraph", F)

    // _variableValues and _weightValues get mutably updated in parallel in the reference implementation.
    // do we want to retain those semantics (and use a concurrent SHashMap) or do something else?

    // _variableFactors connects instantiated variables to factors
    data(FactorGraph, ("_factors", MHashMap(MInt,F)), ("_variables", MHashMap(MInt, Variable)), ("_weights", MHashMap(MInt, Weight)),
                      ("_variableFactors", MHashMap(MInt, DenseVector(MInt))), ("_variableValues", CHashMap(MInt, MDouble)), ("_weightValues", CHashMap(MInt, MDouble)))

    val a = static (FactorGraph) ("apply", F, MethodSignature(List(("factors", MHashMap(MInt,F)), ("variables", MHashMap(MInt, Variable)), ("weights", MHashMap(MInt, Weight)),
                                        ("variableFactors", MHashMap(MInt, DenseVector(MInt))), ("variableValues", CHashMap(MInt, MDouble)), ("weightValues", CHashMap(MInt, MDouble))), FactorGraph))
     // implements allocates(FactorGraph, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})
     impl (a) (allocates(FactorGraph, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5}))


    val FactorGraphOps = withTpe(FactorGraph)
    FactorGraphOps {
      infix ("factorsMap") (Nil :: MHashMap(MInt,F)) implements getter(0, "_factors")
      infix ("variablesMap") (Nil :: MHashMap(MInt, Variable)) implements getter(0, "_variables")
      infix ("weightsMap") (Nil :: MHashMap(MInt, Weight)) implements getter(0, "_weights")
      infix ("variableFactorsMap") (Nil :: MHashMap(MInt, DenseVector(MInt))) implements getter(0, "_variableFactors")
      infix ("variableValuesMap") (Nil :: CHashMap(MInt, MDouble)) implements getter(0, "_variableValues")
      infix ("weightsValuesMap") (Nil :: CHashMap(MInt, MDouble)) implements getter(0, "_weightValues")

      infix ("factors") (Nil :: DenseVector(F)) implements composite ${ densevector_fromarray(fhashmap_values($self.factorsMap), true) }
      infix ("variables") (Nil :: DenseVector(Variable)) implements composite ${ densevector_fromarray(fhashmap_values($self.variablesMap), true) }
      infix ("weights") (Nil :: DenseVector(Weight)) implements composite ${ densevector_fromarray(fhashmap_values($self.weightsMap), true) }

      infix ("getVariableValue") (MethodSignature(List(("id",MInt), ("isPositive",MBoolean,"true")), MDouble)) implements composite ${
        if (isPositive) $self.variableValuesMap.apply(id) else 1.0 - $self.variableValuesMap.apply(id)
      }

      infix ("getWeightValue") (MInt :: MDouble) implements composite ${ $self.weightsValuesMap.apply($1) }

      infix ("updateVariableValue") ((("id",MInt), ("newValue",MDouble)) :: MUnit, effect = write(0)) implements composite ${
        $self.variableValuesMap.update(id, newValue)
      }
      infix ("updateVariableValues") ((("ids",DenseVector(MInt)), ("newValues",DenseVector(MDouble))) :: MUnit, effect = write(0)) implements composite ${
        for (i <- ids.indices) {
          $self.updateVariableValue(ids(i), newValues(i))
        }
      }

      infix ("updateWeightValue") ((("id",MInt), ("newValue",MDouble)) :: MUnit, effect = write(0)) implements composite ${
        $self.weightsValuesMap.update(id, newValue)
      }
      infix ("updateWeightValues") ((("ids",DenseVector(MInt)), ("newValues",DenseVector(MDouble))) :: MUnit, effect = write(0)) implements composite ${
        for (i <- ids.indices) {
          $self.updateWeightValue(ids(i), newValues(i))
        }
      }
    }
  }
}
