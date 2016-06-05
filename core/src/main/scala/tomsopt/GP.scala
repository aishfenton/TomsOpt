package tomsopt

import breeze.linalg._
import breeze.numerics._
import spire.syntax.cfor._
import tomsopt.kernel.Kernel
import tomsopt.utility.Utility
import tomsopt.math._

case class GPModel(mean: DenseVector[Double], cov: DenseMatrix[Double], X: DenseMatrix[Double], t: DenseVector[Double]) {

  lazy val invC: DenseMatrix[Double] = {
    // Yes I know, should'nt be doing inv, but Breeze doesn't have a triangle solver, and is it really that bad?
    // See: http://arxiv.org/abs/1201.6035v1
    val invL = inv(jChol(cov))
    invL.t * invL
  }

  lazy val meanAdjustedT = t - mean

}

class GP(kernel: Kernel, utility: Utility, noise: Double, mean: Double = 0.0) {

  var model: Option[GPModel] = None


  // Kernel wih noise
  @inline
  private def nKernel(x1: DenseVector[Double], x2: DenseVector[Double]) = kernel(x1, x2) + noise

  /**
    * Calculates the kernel between all row/col pairs of matrices Ai kdot Bj. Both matrices are assumed
    * to be in the same orientation when given (i.e. if B is additional rows of A, then it is assumed it already
    * both A and B are rows).
    *
    * @return A kdot B
    */
  @inline
  private def kernelProduct(A: DenseMatrix[Double], B: DenseMatrix[Double]) = {
    val g = DenseMatrix.zeros[Double](A.rows, B.rows)
    val tA = A.t
    val tB = B.t

    cforRange(0 until A.rows) { i =>
      val v = tA(::, i)
      cforRange(0 until B.rows) { j =>
        val k = nKernel(v, tB(::, j))
        g(i, j) = k
      }
    }
    g
  }

  @inline
  private def kernelProduct(A: DenseMatrix[Double], v: DenseVector[Double]) = {
    val g = DenseVector.zeros[Double](A.rows)
    // NB: Expensive, so moved outside loop
    val tA = A.t

    cforRange(0 until A.rows) { i =>
      val k = nKernel(tA(::, i), v)
      g(i) = k
    }
    g
  }

  // Save some computation, and annoyingly need to ensure that same noise is
  // applied to both i,j and j,i
  @inline
  private def kernelProductSym(A: DenseMatrix[Double]) = {
    // NB: Expensive, so moved outside loop
    val tA = A.t

    val g = DenseMatrix.zeros[Double](A.rows, tA.cols)
    cforRange(0 until A.rows) { i =>
      cforRange(i until tA.cols) { j =>
        val k = nKernel(tA(::, i), tA(::, j))
        g(i, j) = k
        g(j, i) = k
      }
    }
    g
  }

  /**
    *  Build a new symmetric matrix from the given blocks.
    *    [  A  B ]
    *    [ B.t C ]
    */
  private def buildMatrix(A: DenseMatrix[Double], B: DenseMatrix[Double], C: DenseMatrix[Double]) = {
    require(A.rows == A.cols && C.rows == C.cols)
    require(B.rows == A.rows && B.cols == C.cols)

    val X = DenseMatrix.zeros[Double](A.rows + B.cols, A.cols + B.cols)

    X(0 until A.rows, 0 until A.cols) := A
    X(0 until A.rows, A.cols until X.cols) := B
    X(A.rows until X.rows, 0 until A.cols) := B.t
    X(A.rows until X.rows, A.cols until X.cols) := C
    X
  }

  def update(X: IndexedSeq[DenseVector[Double]], t: DenseVector[Double]): GP = {
    val mX = DenseMatrix(X: _*)

    val (m, cov, newX, newT) = if (model.isDefined) {
      val existing = model.get
      val m = DenseVector.fill[Double](existing.X.rows + mX.rows, mean)

      val K = kernelProduct(existing.X, mX.t)
      val C = kernelProductSym(mX)
      val cov = buildMatrix(existing.cov, K, C)

      val newX = DenseMatrix.vertcat(existing.X, mX)
      val newT = DenseVector.vertcat(existing.t, t)
      (m, cov, newX, newT)
    } else {
      val m = DenseVector.fill[Double](mX.rows, mean)
      val cov = kernelProductSym(mX)
      (m, cov, mX, t)
    }

    model = Some(GPModel(m, cov, newX, newT))
    this
  }

  def update(x: DenseVector[Double], t: Double): GP = {
    update(Array(x), DenseVector(t))
    this
  }

  /**
    * Decompose matrix as follows:
    * t = [ ta ]
    *     [ ta ]
    *
    * Now we're predicting the distribution given by $$t \sim N(\mu_{b|a}, \Sigma_{b|a})$$
    * Where:
    *   t_{b|a} =
    */

  /**
    * @param X
    * @return
    */
  def predict(X: IndexedSeq[DenseVector[Double]]): Array[(Double, Double)] = {
    X.par.map { v =>
      predict(v)
    }.toArray
  }

  @inline
  def predict(x: DenseVector[Double]) = {
    val m = model.get
    val invC = m.invC

    val k = kernelProduct(m.X, x)
    val c = nKernel(x, x)

//    val tmp: Transpose[DenseVector[Double]] = (invC * k).t
    val tmp: Transpose[DenseVector[Double]] = dsmv(invC, k).t
    val tMean = mean + (tmp * m.meanAdjustedT)
    val tVar = c - (tmp * k)

//    val tMean = mean + (k.t * invC * m.meanAdjustedT)
//    val tVar = c - (k.t * invC * k)

    (tMean, sqrt(tVar))
  }

  def predictBatch(X: DenseMatrix[Double]) = {
    val m = model.get
    val invC = m.invC

    val K = kernelProduct(m.X, X)
    val c: DenseVector[Double] = X(*, ::).map { x => nKernel(x, x) }

    val K2: DenseMatrix[Double] = dsymm(invC, K)
    val tMean = DenseVector.fill[Double](X.rows)(mean) + (m.meanAdjustedT.t * K2).t

    val tVar = DenseVector.zeros[Double](X.rows)
    cforRange(0 until X.rows) { i =>
      tVar(i) = c(i) - ( K2(::, i) dot K(::, i) )
    }

    (tMean, sqrt(tVar))
  }

  def nextBatch(lastBestT: Double, samples: Int = 10000000, blocks: Int = 100): (Double, DenseVector[Double]) = {
    var best: (Double, DenseVector[Double]) = (0.0, null)
    (0 until blocks).par.foreach { i =>
      val X = DenseMatrix.rand[Double](samples / blocks, 7)
      val t = predictBatch(X)

      cforRange(0 until X.rows) { i =>
        val (m, v) = (t._1(i), t._2(i))
        val u = utility(m, v, lastBestT)
        if (u > best._1) best = (m, X(i, ::).inner)
      }
    }

    best
  }

  def next(lastBestT: Double, samples: Int = 10000000): (Double, DenseVector[Double]) = {
    var best: (Double, DenseVector[Double]) = (0.0, null)
    (0 until samples).par.foreach { i =>
      val x = DenseVector.rand[Double](7)
      val t = predict(x)
      val u = utility(t._1, t._2, lastBestT)
      if (u > best._1) best = (t._1, x)
    }

    best
  }

}

