package ppl.dsl.forge
package dsls.optiml
package factor

import core.{ForgeApplication,ForgeApplicationRunner}

trait FactorOps extends TableFactorOps with FunctionFactorOps {
  this: OptiMLDSL =>

  def importAllFactorOps() {
    importFactorVariableOps()
    importVariableFactorOps()
    importTableFactorOps()
    importFunctionFactorOps()
    importFactorOps()
  }

  def importFactorVariableOps() {
    val DenseVector = lookupTpe("DenseVector")

    val FVariable = tpe("FactorVariable")
    data(FVariable, ("_id", MInt), ("_isPositive", MBoolean), ("_position", MInt))

    static (FVariable) ("apply", Nil, (("id", MInt), ("isPositive", MBoolean), ("position", MInt)) :: FVariable) implements allocates(FVariable, ${$0}, ${$1}, ${$2})

    val FVariableOps = withTpe(FVariable)
    FVariableOps {
      infix ("id") (Nil :: MInt) implements getter(0, "_id")
      infix ("isPositive") (Nil :: MBoolean) implements getter(0, "_isPositive")
      infix ("position") (Nil :: MInt) implements getter(0, "_position")
    }
  }

  def importVariableFactorOps() {
    val DenseVector = lookupTpe("DenseVector")
    val FactorVariable = lookupTpe("FactorVariable")
    val Tup2 = lookupTpe("Tup2")

    val VFactor = tpe("VariableFactor")
    data(VFactor, ("_id", MInt), ("_funcId", MInt), ("_nVariables", MInt), ("_iStart", MInt), ("_weightId", MInt))

    static (VFactor) ("apply", Nil, (("id", MInt), ("funcId", MInt), ("nVariables", MInt), ("iStart", MInt), ("weightId", MInt)) :: VFactor) implements allocates(VFactor, ${$0}, ${$1}, ${$2}, ${$3}, ${$4})

    compiler (VFactor) ("getValue", Nil, (("isPositive", MBoolean), ("value", MDouble)) :: MDouble) implements composite ${
      if (isPositive) 
        value
      else 
        1.0 - value
    }

    compiler (VFactor) ("or_factor", Nil, (("nVariables", MInt), ("iStart", MInt), ("vals", DenseVector(MDouble)), ("factorsToVariables", DenseVector(FactorVariable)), ("variableId", MInt)) :: Tup2(MDouble, MBoolean)) implements composite ${
      if (nVariables == 0) pack((unit(1.0), unit(true)))
      else {
        var i = 0
        var flag = true
        var isPositive = true
        while (i < nVariables && flag) {
          val variable = factorsToVariables(iStart + i)
          flag = (getValue(variable.isPositive, vals(variable.id)) == 0.0)
          if (variable.id == variableId) {
            isPositive = variable.isPositive
          }
          i += 1
        }
        if (flag) {
          pack((unit(0.0), isPositive))
        }
        else {
          pack((unit(1.0), isPositive))
        }
      }
    }

    compiler (VFactor) ("and_factor", Nil, (("nVariables", MInt), ("iStart", MInt), ("vals", DenseVector(MDouble)), ("factorsToVariables", DenseVector(FactorVariable)), ("variableId", MInt)) :: Tup2(MDouble, MBoolean)) implements composite ${
      if (nVariables == 0) pack((unit(1.0), unit(true)))
      else {
        var i = 0
        var flag = true
        var isPositive = true
        while (i < nVariables && flag) {
          val variable = factorsToVariables(iStart + i)
          flag = (getValue(variable.isPositive, vals(variable.id)) != 0.0)
          if (variable.id == variableId) {
            isPositive = variable.isPositive
          }
          i += 1
        }
        if (flag) {
          pack((unit(1.0), isPositive))
        }
        else {
          pack((unit(0.0), isPositive))
        }
      }
    }

    compiler (VFactor) ("imply_factor", Nil, (("nVariables", MInt), ("iStart", MInt), ("vals", DenseVector(MDouble)), ("factorsToVariables", DenseVector(FactorVariable)), ("variableId", MInt)) :: Tup2(MDouble, MBoolean)) implements composite ${
      if (nVariables == 1) {
        val variable = factorsToVariables(iStart)
        if (variable.id == variableId) {
          pack((getValue(variable.isPositive, vals(variable.id)), variable.isPositive))
        }
        else fatal("cannot evaluate imply: variableId not match")
      }
      else {
        var i = 0
        var flag = true
        var isPositive = true
        while (i < nVariables - 1 && flag) {
          val variable = factorsToVariables(iStart + i)
          flag = (getValue(variable.isPositive, vals(variable.id)) != 0.0)
          if (variable.id == variableId) {
            isPositive = variable.isPositive
          }
          i += 1
        }
        val variable = factorsToVariables(iStart + nVariables - 1)
        if (variable.id == variableId) {
          isPositive = variable.isPositive
        }
        if (flag) {
          pack((getValue(variable.isPositive, vals(variable.id)), isPositive))
        }
        else {
          pack((unit(1.0), isPositive))
        }
      }
    }

    compiler (VFactor) ("equal_factor", Nil, (("nVariables", MInt), ("iStart", MInt), ("vals", DenseVector(MDouble)), ("factorsToVariables", DenseVector(FactorVariable)), ("variableId", MInt)) :: Tup2(MDouble, MBoolean)) implements composite ${
      if (nVariables == 2) {
        val var1 = factorsToVariables(iStart)
        val var2 = factorsToVariables(iStart + 1)
        val value1 = getValue(var1.isPositive, vals(var1.id))
        val value2 = getValue(var2.isPositive, vals(var2.id))
        val isPositive = {
          if (var1.id == variableId) {
            var1.isPositive
          }
          else if (var2.id == variableId) {
            var2.isPositive
          }
          else fatal("cannot evaluate equality: variableId not match")
        }
        if (value1 == value2) pack((unit(1.0), isPositive))
        else pack((unit(0.0), isPositive))
      }
      else {
        fatal("cannot evaluate equality between more than 2 variables")
      }
    }

    compiler (VFactor) ("istrue_factor", Nil, (("nVariables", MInt), ("iStart", MInt), ("vals", DenseVector(MDouble)), ("factorsToVariables", DenseVector(FactorVariable)), ("variableId", MInt)) :: Tup2(MDouble, MBoolean)) implements composite ${
      if (nVariables == 1) {
        val variable = factorsToVariables(iStart)
        if (variable.id == variableId) {
          pack((getValue(variable.isPositive, vals(variable.id)), variable.isPositive))
        }
        else fatal("cannot evaluate isTrue: variableId not match")
      }
      else fatal("cannot evaluate isTrue for more than 1 variable")
    }


    compiler (VFactor) ("evaluate_factor", Nil, MethodSignature(List(("funcId", MInt), ("nVariables", MInt), ("iStart", MInt), ("vals", DenseVector(MDouble)), ("factorsToVariables", DenseVector(FactorVariable)), ("variableId", MInt)), Tup2(MDouble, MBoolean))) implements composite ${
      // if the conditional is known at staging time, we can inline the exact function
      // and as a consequence, the back-end 'funcId' field in the factor should be DFE'd
      if (funcId == 0) {
        imply_factor(nVariables, iStart, vals, factorsToVariables, variableId)
      }
      else if (funcId == 1) {
        or_factor(nVariables, iStart, vals, factorsToVariables, variableId)
      }
      else if (funcId == 2) {
        and_factor(nVariables, iStart, vals, factorsToVariables, variableId)
      }
      else if (funcId == 3) {
        equal_factor(nVariables, iStart, vals, factorsToVariables, variableId)
      }
      else if (funcId == 4) {
        istrue_factor(nVariables, iStart, vals, factorsToVariables, variableId)
      }
      else {
        fatal("no factor func with id " + funcId + " found")
      }
    }

    
    val VFactorOps = withTpe(VFactor)
    VFactorOps {
      infix ("id") (Nil :: MInt) implements getter(0, "_id")
      infix ("funcId") (Nil :: MInt) implements getter(0, "_funcId")
      infix ("nVariables") (Nil :: MInt) implements getter(0, "_nVariables")
      infix ("iStart") (Nil :: MInt) implements getter(0, "_iStart")
      infix ("weightId") (Nil :: MInt) implements getter(0, "_weightId")
      infix ("evaluate") (MethodSignature(List(("variableValues", DenseVector(MDouble)), ("factorsToVariables", DenseVector(FactorVariable)), ("variableId", MInt)), Tup2(MDouble, MBoolean))) implements composite ${ evaluate_factor($self.funcId, $self.nVariables, $self.iStart, variableValues, factorsToVariables, variableId) }
    }
  }

  // -- Factor type-class
  // the main issue with this organization is that we cannot store multiple factor types in a single graph
  // unless we store a separate map per factor type. we should look into an interface / struct inheritance model.

  object TFactor extends TypeClassSignature {
    def name = "Factor"
    def prefix = "_fact"
    def wrapper = Some("facttype")
  }

  def importFactorOps() {
    val T = tpePar("T")
    val DenseVector = lookupTpe("DenseVector")
    val FVariable = lookupTpe("FactorVariable")

    val Factor = tpeClass("Factor", TFactor, T)

    // Factor interface
    infix (Factor) ("vars", T, T :: DenseVector(FVariable))
    infix (Factor) ("valueOfAssignment", T, (T, DenseVector(MDouble)) :: MDouble)
    infix (Factor) ("weightId", T, T :: MDouble)

    // TableFactor impl
    //val TableFactor = lookupTpe("TableFactor")
    //val FactorTableFactor = tpeClassInst("FactorTableFactor", Nil, Factor(TableFactor))

    //infix (FactorTableFactor) ("vars", Nil, TableFactor :: DenseVector(FVariable)) implements composite ${ $0.vars }
    //infix (FactorTableFactor) ("valueOfAssignment", Nil, (TableFactor, DenseVector(MDouble)) :: MDouble) implements composite ${
      // this is slow, since we need to figure out the logical assignment from the value assignment
      // is there a better way?
      //val assignment = $0.vars.map(_.domain).zip($1) { (domain, value) => domain.find(_ == value).first }
      //val index = assignmentToIndex(assignment)
      //$0.vals.apply(index)
    //}
    //infix (FactorTableFactor) ("weightId", Nil, TableFactor :: MDouble) implements composite ${ unit(0.0) } // TODO

    // FunctionFactor impl
    val FunctionFactor = lookupTpe("FunctionFactor")
    val FactorFunctionFactor = tpeClassInst("FactorFunctionFactor", Nil, Factor(FunctionFactor))

    infix (FactorFunctionFactor) ("vars", Nil, FunctionFactor :: DenseVector(FVariable)) implements composite ${ $0.vars }
    infix (FactorFunctionFactor) ("valueOfAssignment", Nil, (FunctionFactor, DenseVector(MDouble)) :: MDouble) implements composite ${
      $0.evaluate($1)
    }
    infix (FactorFunctionFactor) ("weightId", Nil, FunctionFactor :: MDouble) implements composite ${ $0.weightId }
  }
}
