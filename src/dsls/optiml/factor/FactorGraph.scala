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
    data(Variable, ("_id", MInt), ("_lowBound", MDouble), ("_upBound", MDouble), ("_value", MDouble), ("_isEvidence", MBoolean), ("_nFactors", MInt), ("_iStart", MInt))

    static (Variable) ("apply", Nil, MethodSignature(List(("id", MInt), ("lowBound", MDouble), ("upBound", MDouble), ("value", MDouble), ("isEvidence", MBoolean), ("nFactors", MInt), ("iStart", MInt)), Variable)) implements
      allocates(Variable, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5}, ${$6})

    // how do we add Ordering[Variable] in Forge? no way in Forge to get Ordering(Variable) now, since Ordering is built-in
    // currently we can work-around this using sortBy, but this should really be possible in general..
    
    // val VariableOrdering = tpeClassInst("OrderingVariable", T withBound TOrdering, Ordering(Variable))
    // infix (DenseVectorArith) ("zero", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ DenseVector[T]($0.length,$0.isRow).unsafeImmutable }
    // infix (DenseVectorArith) ("empty", T withBound TArith, Nil :: DenseVector(T)) implements composite ${ DenseVector[T](unit(0),unit(true)).unsafeImmutable }
    
    val VariableOps = withTpe(Variable)
    VariableOps {
      infix ("id") (Nil :: MInt) implements getter(0, "_id")
      infix ("lowBound") (Nil :: MDouble) implements getter(0, "_lowBound")
      infix ("upBound") (Nil :: MDouble) implements getter(0, "_upBound")
      infix ("value") (Nil :: MDouble) implements getter(0, "_value")
      infix ("isEvidence") (Nil :: MBoolean) implements getter(0, "_isEvidence")
      infix ("nFactors") (Nil :: MInt) implements getter(0, "_nFactors")
      infix ("iStart") (Nil :: MInt) implements getter(0, "_iStart")
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
    val Variable = lookupTpe("RandomVariable")
    val Weight = lookupTpe("Weight")
    //val F = tpePar("F") withBound TFactor
    val FactorGraph = tpe("FactorGraph")//, F)
    val VariableFactor = lookupTpe("VariableFactor")
    val FactorVariable = lookupTpe("FactorVariable")

    // we require a dense id space for factors, variables, and weights (from 0 to numX) so that we can store them as vectors instead of maps
    // (deepdive input format must have changed: previously weight ids were sparse (and could even be negative), variable ids were also sparse)

    data(FactorGraph, ("_factors", DenseVector(VariableFactor)), ("_variables", DenseVector(Variable)), ("_weights", DenseVector(Weight)),
                      ("_variablesToFactors", DenseVector(VariableFactor)), ("_factorsToVariables", DenseVector(FactorVariable)), ("_variableValues", DenseVector(MDouble)), ("_weightValues", DenseVector(MDouble)))

    // all input vectors must be sorted by id!
    val a = static (FactorGraph) ("apply", Nil, MethodSignature(List(("factors", DenseVector(VariableFactor)), ("variables", DenseVector(Variable)), ("weights", DenseVector(Weight)),
                                                                   ("variablesToFactors", DenseVector(VariableFactor)), ("factorsToVariables", DenseVector(FactorVariable)), ("variableValues", DenseVector(MDouble)), ("weightValues", DenseVector(MDouble))), FactorGraph))
     // implements allocates(FactorGraph, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})
     impl (a) (allocates(FactorGraph, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5}, ${$6}))


    val FactorGraphOps = withTpe(FactorGraph)
    FactorGraphOps {      
      infix ("factors") (Nil :: DenseVector(VariableFactor)) implements getter(0, "_factors")
      infix ("variables") (Nil :: DenseVector(Variable)) implements getter(0, "_variables")
      infix ("weights") (Nil :: DenseVector(Weight)) implements getter(0, "_weights")
      infix ("variablesToFactors") (Nil :: DenseVector(VariableFactor)) implements getter(0, "_variablesToFactors")
      infix ("factorsToVariables") (Nil :: DenseVector(FactorVariable)) implements getter(0, "_factorsToVariables")
      infix ("variableValues") (Nil :: DenseVector(MDouble)) implements getter(0, "_variableValues")
      //compiler ("infix_variableValues") (Nil :: DenseVector(MDouble)) implements getter(0, "_variableValues")
      compiler ("infix_weightsValues") (Nil :: DenseVector(MDouble)) implements getter(0, "_weightValues")

      infix ("getVariableValue") (MethodSignature(List(("id",MInt), ("isPositive",MBoolean,"unit(true)")), MDouble)) implements composite ${
        if (isPositive) $self.variableValues.apply(id) else 1.0 - $self.variableValues.apply(id)
      }

      infix ("getWeightValue") (MInt :: MDouble) implements composite ${ $self.weightsValues.apply($1) }

      infix ("updateVariableValue") ((("id",MInt), ("newValue",MDouble)) :: MUnit, effect = write(0)) implements composite ${
        $self.variableValues(id) = newValue
      }
      infix ("updateVariableValues") ((("ids",DenseVector(MInt)), ("newValues",DenseVector(MDouble))) :: MUnit, effect = write(0)) implements composite ${
        for (i <- ids.indices) {
          $self.updateVariableValue(ids(i), newValues(i))
        }
      }

      infix ("updateWeightValue") ((("id",MInt), ("newValue",MDouble)) :: MUnit, effect = write(0)) implements composite ${
        $self.weightsValues(id) = newValue
      }
      infix ("updateWeightValues") ((("ids",DenseVector(MInt)), ("newValues",DenseVector(MDouble))) :: MUnit, effect = write(0)) implements composite ${
        for (i <- ids.indices) {
          $self.updateWeightValue(ids(i), newValues(i))
        }
      }
    }
  }
}