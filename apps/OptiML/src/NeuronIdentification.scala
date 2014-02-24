import optiml.compiler._
import optiml.library._
import optiml.shared._

import scala.virtualization.lms.common.Record

object NeuronIdCompiler extends OptiMLApplicationCompiler with NeuronId
object NeuronIdInterpreter extends OptiMLApplicationInterpreter with NeuronId

trait NeuronId extends OptiMLApplication {
//gauss image
  def gaussImage(mux: Rep[Double], muy: Rep[Double], sigx: Rep[Double], sigy: Rep[Double], theta: Rep[Double], imgHeight: Rep[Int], imgWidth: Rep[Int]): Rep[DenseMatrix[Double]] = {
    val a = (cos(theta)~^2)/(2*sigx~^2)+(sin(theta)~^2)/(2*sigy~^2)
    val b = -sin(2*theta)/(4*sigx~^2)+sin(2*theta)/(4*sigy~^2)
    val c = (sin(theta)~^2)/(2*sigx~^2)+(cos(theta)~^2)/(2*sigy~^2)

    (1::imgHeight+1, 1::imgWidth+1) { (i,j) =>
        exp(-((a*(j-mux)~^2)
            + 2*b*(j-mux)*(i-muy)
            + c*(i-muy)~^2))
    }
  }


  def calcCellImgs(muX: Rep[DenseVector[Double]], muY: Rep[DenseVector[Double]], sigX: Rep[DenseVector[Double]], sigY: Rep[DenseVector[Double]], theta: Rep[DenseVector[Double]], imgHeight: Rep[Int], imgWidth: Rep[Int]): Rep[DenseVector[DenseMatrix[Double]]] = {
    val numImgs = muX.length // == nCells
    (0::numImgs) { i =>
      gaussImage(muX(i), muY(i), sigX(i), sigY(i), theta(i), imgHeight, imgWidth)
    }
  }


  def calcCellImgsByParam(muXVals: Rep[DenseMatrix[Double]], muYVals: Rep[DenseMatrix[Double]], sigXVals: Rep[DenseMatrix[Double]], sigYVals: Rep[DenseMatrix[Double]], thetaVals: Rep[DenseMatrix[Double]], nCells: Rep[Int], imgHeight: Rep[Int], imgWidth: Rep[Int]): Rep[DenseVector[DenseMatrix[Double]]] = 
      {
        val numMuVals = muXVals.numCols
        val numSigVals = sigXVals.numCols
        val numThetaVals = thetaVals.numCols
        //Question: cumulative product? paramSizeCumProd=[1 cumprod([numMuVals, numMuVals, numSigVals, numSigVals])];
        val prod = (numMuVals~^2)*(numSigVals~^2)*numThetaVals
        val cellImgsByParam = (0::imgSize, 0::nCells, 0::prod) { (x, y, z) => 0 }
//**Question: stacked for loops 
        (0::nCells, 0::numMuVals, 0::numMuVals, 0::numSigVals, 0::numSigVals, 0::numThetaVals) {
            (cInd, muInd1, muInd2, sigInd1, sigInd2, thetaInd) =>
            val lInd = (DenseVector(muInd1, muInd2, sigInd1, sigInd2, thetaInd) - 1)*:*(paramSizeCumProd) + 1;
//**Question: setting this part of the matrix: cellImgsByParam(:,:,cInd,linInd) to this line
               calcCellImgs(muXVals(cInd,muInd1), muYVals(cInd, muInd2), sigXVals(cInd, sigInd1), sigYVals(cInd, sigInd2), thetaVals(cInd,thetaInd), imgHeight, imgWidth)
        }
        (cellImgsByParam)
  }

  def getCellNeighbors(cellImgs: Rep[DenseVector[DenseMatrix[Double]]]): (Rep[DenseVector[DenseVector[Double]]], Rep[DenseMatrix[Double]]) = {
    val nCells = cellImgs(0).numCols
    val cellImgOverlaps = calcCellImgOverlaps(cellImgs)
    val neighbors = (0::nCells) { (i) =>
        cellImgOverlaps(0,0) = 0; cellImgOverlaps.getRow(i).find(_ > 0.15)
        }
    (neighbors, cellImgOverlaps)
  }
  
  def getAllfVals_singleCluster(thisft: Rep[DenseVectorView[Double]], activeCells: Rep[Double], fOffsetVec: Rep[DenseVector[Double]], nCells: Rep[Double]): (Rep[DenseMatrix[Double]]) = {
     // number of combinations of fvals is determined by the number of active cells
    val numfvecs = pow(fOffsetVec.length, activeCells.length).toInt

    // construct a consecutive numbering to allow us to quickly determine an assignment later
    val activeCellIds = (0::nCells) { i => val loc = activeCells.find(_ == i); if (loc.length > 0) loc(0) else -1 }

    // allfvals corresponds to a unique assignment of nCells, 1 assignment per fVal
    val allfvals = (0::nCells, 0::numfvecs) { (i,j) =>
      val id = activeCellIds(i)
      if (id > -1) {
        // now need to take j and determine which index of fOffsetVec to select for this assignment

        // e.g. if there are 3 active cells, the index assignments look like:
        //   c1 c2 c3
        //   1  1  1
        //   2  1  1
        //   ...
        //   7  1  1
        //   1  2  1

        val sel = (j / pow(fOffsetVec.length,id).toInt) % fOffsetVec.length
        thisft(i)+fOffsetVec(sel)
      }
      else {
        thisft(i)
      }
    }

    (allfvals)
  }
  
  def calcCellImgOverlaps(cellImgs: Rep[DenseVector[DenseMatrix[Double]]]):
      (Rep[DenseMatrix[Double]]) = {
    val nCells = cellImgs(0).numCols;

    val cellImgOverlaps = (0::nCells, 0::nCells) { (x,y) => 1 }
    (0::nCells) { (cInd) =>
        /** Question: Accessing **/
    
    }
    (cellImgOverlaps)
  }


  def getCellImg(cellImgsPermuted: Rep[DenseVector[DenseMatrix[Double]]], c: Rep[Int]) = {
    val nxpix = cellImgsPermuted.length
    val nypix = cellImgsPermuted(0).numRows

    (0::nypix, 0::nxpix) { (y,x) =>
      cellImgsPermuted(x).apply(y,c)
    }
  }


  def replaceCellImgInPlace(cellImgsPermuted: Rep[DenseVector[DenseMatrix[Double]]], c: Rep[Int], thisLoopCellImg: Rep[DenseMatrix[Double]]) = {
    val nxpix = cellImgsPermuted.length
    val nypix = cellImgsPermuted(0).numRows

    for (x <- 0::nxpix) {
      val m: Rep[DenseMatrix[Double]] = cellImgsPermuted(x)
      for (y <- 0::nypix) {
        m(y, c) = thisLoopCellImg(y,x)
      }
    }
    ()
  }


 def logpFgivenf(F: Rep[DenseMatrix[Double]], allfvals: Rep[DenseMatrix[Double]], cellImgs: Rep[DenseVector[DenseMatrix[Double]]], noiseSigma: Rep[Double], bg: Rep[DenseMatrix[Double]]): Rep[DenseVector[Double]] = {
    val numfvecs = allfvals.numCols
    //Question: pi in matlab>
    val logSigmaTerm = log(0.01) - log(sqrt(2 * 3.14) * noiseSigma)
    (0::numfvecs) { i =>
        val thisBG = bg + cellImgs*allfvals.getCol(i);
        partialLogp = -0.5 * (((F - thisBG)/noiseSigma)~^2)+logSigmaTerm;
        sum(sum(partialLogp))/F.numRows
    }
 }

  def logLik(imgs: Rep[DenseVector[DenseMatrix[Double]]],
             noiseSigma: Rep[Double],
             bg: Rep[DenseMatrix[Double]],
             thisf: Rep[DenseMatrix[Double]],
             thisMuX: Rep[DenseVector[Double]],
             thisMuY: Rep[DenseVector[Double]],
             thisSigX: Rep[DenseVector[Double]],
             thisSigY: Rep[DenseVector[Double]],
             thisTheta: Rep[DenseVector[Double]],
             numfvals: Rep[Int],
             muXVals: Rep[DenseMatrix[Double]],
             muYVals: Rep[DenseMatrix[Double]],
             sigXVals: Rep[DenseMatrix[Double]],
             sigYVals: Rep[DenseMatrix[Double]],
             thetaVals: Rep[DenseMatrix[Double]],
             fInc: Rep[Double]) = {
                 

    // preallocate log-likelihood matrix
    val nCells = thisSigX.length
    val numMuVals = muXVals.numCols
    val numSigVals = sigXVals.numCols
    val numThetaVals = thetaVals.numCols

    // log-likelihood parameters
    val muX = DenseMatrix[Double](nCells,numMuVals)
    val muY = DenseMatrix[Double](nCells,numMuVals)
    val sigX = DenseMatrix[Double](nCells,numSigVals)
    val sigY = DenseMatrix[Double](nCells,numSigVals)
    val theta = DenseMatrix[Double](nCells,numThetaVals)

    // calculate cell images for all parameter combination
    val imgHeight = imgs(0).numRows
    val imgWidth = imgs(0).numCols
    val cellImgsByParam = calcCellImgsByParam(muXVals, muYVals, sigXVals, sigYVals, thetaVals, nCells, imgHeight, imgWidth)

    // get indices of current parameter guesses
    val thisMuXInds = (0::nCells) { i => muXVals(i).find(e => round(10000*e) == round(10000*thisMuX(i))).apply(0) }
    val thisMuYInds = (0::nCells) { i => muYVals(i).find(e => round(10000*e) == round(10000*thisMuY(i))).apply(0) }
    val thisSigXInds = (0::nCells) { i => sigXVals(i).find(e => round(10000*e) == round(10000*thisSigX(i))).apply(0) }
    val thisSigYInds = (0::nCells) { i => sigYVals(i).find(e => round(10000*e) == round(10000*thisSigY(i))).apply(0) }
    val thisThetaInds = (0::nCells) { i => thetaVals(i).find(e => round(10000*e) == round(10000*thisTheta(i))).apply(0) }

//** clear muXVals muYVals sigXVals sigYVals thetaVals
    // initialize cell images for current parameter guesses
    val cellImgs = calcCellImgs(thisMuX, thisMuY, thisSigX, thisSigY, thisTheta, imgHeight, imgWidth)
    val (neighbors, cellImgOverlaps) = getCellNeighbors(cellImgs)
    val (cvxHulls, areas, binImages) = getConvexHull(cellImgs)

//Question: Reshaping/flattening? //reshape everything
   //cellImgs=reshape(cellImgs, [imgSize(1)*imgSize(2), nCells]);
   //cellImgsByParam=reshape(cellImgsByParam, [imgSize(1)*imgSize(2), nCells, size(cellImgsByParam,4)]);
   //bg=reshape(bg, [imgSize(1)*imgSize(2), 1]);

    //
    val numSigmasThresh = 3
    val (singleCellActiveTimes,activeTimes) = calcSingleCellActiveTimes(thisf, numSigmasThresh, noiseSigma, neighbors);
    //minimum number of frames to use to estimate the cell shape
    val minNumActiveTimes=15;
    val maxNumActiveTimes=40;
    
    // set the vector used for the conditional f dist
    val fOffsetVec = (-ceil(numfvals/2.0) :: ceil(numfvals/2.0)+1).toDouble*fInc
//** clear numfvals fInc
    // loop over cells and do EM on each separately, during only its solo or
    // nearly solo active times
    for(cInd <- (0::nCells)) {
        //check to see whether this cell has enough times when it is active by
        //itself (no neighbors active)
        //then pick the best active times to use, depending on condition
        val (theseActiveTimes, neighborsActiveOk) = findBestActiveTimes(cInd,singleCellActiveTimes, activeTimes, minNumActiveTimes, maxNumActiveTimes, neighbors, thisf, noiseSigma)
        // val (meanNeighborRatios, ratioToMeanNeighbor, meanNeighborDifference) = calculateMeanNeighborRatio(cInd, theseActiveTimes, neighbors, thisf)
        //if there are no active times, set the likelihood of the current
        //parameters to > the max value
        if(theseActiveTimes.length < 1) {
            muX(cInd,thisMuXInds(cInd)) =1;
            muY(cInd,thisMuYInds(cInd))=1;
            sigX(cInd,thisSigXInds(cInd))=1;
            sigY(cInd,thisSigYInds(cInd))=1;
            theta(cInd,thisThetaInds(cInd))=1;
        }
        else if(theseActiveTimes.numRows > 1) {
            theseActiveTimes = theseActiveTimes.t
        }  
        if(theseActiveTimes.numRows > 1) { 
            //error reporting?:   error('theseActiveTimes is the wrong size....')
        }
      //now go through the active times and add to the likelihood of each
      //shape parameter
      val tInd = 0
      for(t <- theseActiveTimes) {
      val F = imgs(t)
      //**  F=reshape(F, [imgSize(1)*imgSize(2), 1]); 
      val thisft = thisf.getCol(t)
//    get the options for fval combos for this time: commented out in matlab code
//        activeCells=[neighbors{cInd}(thisft(neighbors{cInd})>numSigmasThresh*noiseSigma), cInd];
//        if length(activeCells)>1
//            if ~neighborsActiveOk
//                error('Something went wrong in finding active cells.')
//            end
//            if size(activeCells,1)>1
//                activeCells=activeCells';
//            end
//        end
        val activeCells=cInd;
	 // actual EM
        val allfvals = getAllfVals_singleCluster(thisft,activeCells,fOffsetVec,nCells);

        //E
        val thisLoopCellImgs=cellImgs;
        // q(f') = log( p(f=f'|F;sigmas, mus) ) = logp*p(f=f';sigmas,mus)/p(F;sigmas,mus)
        // where f' is a specific vector of f values for all cells
        // elopq exp(q*p(F)/p(f=f'))
        val elogp = exp(logpFgivenf(F, allfvals, thisLoopCellImgs, noiseSigma, bg));
        //*** M-step: if variables are reshaped properly, how to do:
        //linInd=sum(([muInd thisMuYInds(cInd) thisSigXInds(cInd) thisSigYInds(cInd) thisThetaInds(cInd)]-1).*paramSizeCumProd)+1;
        //thisLoopCellImgs.getCol(cInd)=cellImgsByParam(:,cInd,linInd);
        //logpThisParam=logpFgivenf(F, allfvals, thisLoopCellImgs, noiseSigma, bg);
        //logLik{1}(cInd,muInd)=logLik{1}(cInd,muInd)+sum(logpThisParam.*elogp);
	   for (muInd <- (0::numMuVals)) {
	          // muX
        	  val idxX = flatten((c, muInd, thisMuYInds(c), thisSigXInds(c), thisSigYInds(c), thisThetaInds(c)), (nCells, muXVals.numCols, muYVals.numCols, sigXVals.numCols, sigYVals.numCols, thetaVals.numCols))
          	  replaceCellImgInPlace(cellImgsPermuted, c, cellImgsByParam(idxX))
          	  val logpThisParamX = logpFgivenfwithCellImgs_allfvals_diffVals(F, allfvals, cellImgsPermuted, noiseSigma, bg)
          	  muX(c,muInd) = muX(c,muInd) + sum(logpThisParamX*elogp)

          	  // muY
         	  val idxY = flatten((c, thisMuXInds(c), muInd, thisSigXInds(c), thisSigYInds(c), thisThetaInds(c)), (nCells, muXVals.numCols, muYVals.numCols, sigXVals.numCols, sigYVals.numCols, thetaVals.numCols))
          	  replaceCellImgInPlace(cellImgsPermuted, c, cellImgsByParam(idxY))
          	  val logpThisParamY = logpFgivenfwithCellImgs_allfvals_diffVals(F, allfvals, cellImgsPermuted, noiseSigma, bg)
          	  muY(c,muInd) = muY(c,muInd) + sum(logpThisParamY*elogp)
           }

           for (sigInd <- (0::numSigVals)) {
         	  // sigX
          	  val idxX = flatten((c, thisMuXInds(c), thisMuYInds(c), sigInd, thisSigYInds(c), thisThetaInds(c)), (nCells, muXVals.numCols, muYVals.numCols, sigXVals.numCols, sigYVals.numCols, thetaVals.numCols))
          	  replaceCellImgInPlace(cellImgsPermuted, c, cellImgsByParam(idxX))
               	  val logpThisParamX = logpFgivenfwithCellImgs_allfvals_diffVals(F, allfvals, cellImgsPermuted, noiseSigma, bg)
          	  sigX(c,sigInd) = sigX(c,sigInd) + sum(logpThisParamX*elogp)

          	  // sigY
          	  val idxY = flatten((c, thisMuXInds(c), thisMuYInds(c), thisSigXInds(c), sigInd, thisThetaInds(c)), (nCells, muXVals.numCols, muYVals.numCols, sigXVals.numCols, sigYVals.numCols, thetaVals.numCols))
          	  replaceCellImgInPlace(cellImgsPermuted, c, cellImgsByParam(idxY))
          	  val logpThisParamY = logpFgivenfwithCellImgs_allfvals_diffVals(F, allfvals, cellImgsPermuted, noiseSigma, bg)
          	  sigY(c,sigInd) = sigY(c,sigInd) + sum(logpThisParamY*elogp)
           }
         
           for (thetaInd <- (0::numThetaVals)) {
         	  // theta
          	  val idx = flatten((c, thisMuXInds(c), thisMuYInds(c), thisSigXInds(c), thisSigYInds(c), thetaInd), (nCells, muXVals.numCols, muYVals.numCols, sigXVals.numCols, sigYVals.numCols, thetaVals.numCols))
          	  replaceCellImgInPlace(cellImgsPermuted, c, cellImgsByParam(idx))
          	  val logpThisParam = logpFgivenfwithCellImgs_allfvals_diffVals(F, allfvals, cellImgsPermuted, noiseSigma, bg)
          	  theta(c,thetaInd) = theta(c,thetaInd) + sum(logpThisParam*elogp)
           }
       }
    }
    pack(muX, muY, sigX, sigY, theta)
             
  }

  def main() {
    // no nice binary format for optiml yet, so we pass in data as multiple files using a naming convention
    val srcDir = args(0)
    val numImgs = args(1).toInt

    // val imgs = (1::numImgs+1) { i => readMatrix(srcDir + "_img_" + i + ".dat") }
    // workaround for inline i/o Delite activation record bug
    val imgsBuild = DenseVector[DenseMatrix[Double]](numImgs, true)
    var i = 0
    while (i < numImgs) {
      imgsBuild(i) = readMatrix(srcDir + "_img_" + (i+1) + ".dat")
      i += 1
    }
    val imgs = imgsBuild.unsafeImmutable

    val noiseSigma = readVector(srcDir + "_noiseSigma.dat").apply(0)
    val bg = readMatrix(srcDir + "_bg.dat")
    val thisf = readMatrix(srcDir + "_thisf.dat")
    val thisMuX = readVector(srcDir + "_thisMuX.dat").t
    val thisMuY = readVector(srcDir + "_thisMuY.dat").t
    val thisSigX = readVector(srcDir + "_thisSigX.dat").t
    val thisSigY = readVector(srcDir + "_thisSigY.dat").t
    val thisTheta = readVector(srcDir + "_thisTheta.dat").t
    val numfvals = readVector(srcDir + "_numfvals.dat").apply(0).toInt
    val muXVals = readMatrix(srcDir + "_muXVals.dat")
    val muYVals = readMatrix(srcDir + "_muYVals.dat")
    val sigXVals = readMatrix(srcDir + "_sigXVals.dat")
    val sigYVals = readMatrix(srcDir + "_sigYVals.dat")
    val thetaVals = readMatrix(srcDir + "_thetaVals.dat")
    val fInc = readVector(srcDir + "_finc.dat").apply(0)

    println("finished loading input...")

    tic()
    val a = logLik(imgs, noiseSigma, bg, thisf, thisMuX, thisMuY, thisSigX, thisSigY, thisTheta, numfvals, muXVals, muYVals, sigXVals, sigYVals, thetaVals, fInc)
    toc(a)

    println("log likelihood finished. saving parameters...")

    val (muX, muY, sigX, sigY, theta) = unpack(a)

    println("muX(0,0): " + muX(0,0))
    println("muY(0,0): " + muY(0,0))
    println("sigX(0,0): " + sigX(0,0))
    println("sigY(0,0): " + sigY(0,0))
    println("theta(0,0): " + theta(0,0))

    writeMatrix(muX, srcDir + "_mux.out")
    writeMatrix(muY, srcDir + "_muy.out")
    writeMatrix(sigX, srcDir + "_sigx.out")
    writeMatrix(sigY, srcDir + "_sigy.out")
    writeMatrix(theta, srcDir + "_sigtheta.out")
  }
}
