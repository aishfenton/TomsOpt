package tomsopt

import breeze.linalg._
import breeze.numerics._
import spire.syntax.cfor._
import tomsopt.kernel._
import tomsopt.utility.Utility
import tomsopt.math._

/**
  * Immutable model holding the current state of the GP. Also contains pre-cached versions of inverse Covariance
  * matrix, and (t - mean).
  */
case class GPModel(mean: DenseVector[Double], cov: DenseMatrix[Double], X: DenseMatrix[Double], t: DenseVector[Double]) {

  /**
    * Inverted covariance matrix.
    */
  lazy val invC: DenseMatrix[Double] = {
    // Yes I know, numeric stability of using an inv, but is it really that bad?
    // See: http://arxiv.org/abs/1201.6035v1
    val invL = inv(jChol(cov))
    invL.t * invL
  }

  /**
    * Observations, t, minus the model's mean.
    */
  lazy val tMinusMean = t - mean

}

class GaussianProcess(kernel: Kernel, utility: Utility, noise: Double, mean: Double = 0.0,
                      kernelUpdater: KernelUpdater = new KernelUpdaterArray) {

  var model: Option[GPModel] = None

  private def updateKernel(A: DenseMatrix[Double], B: DenseMatrix[Double]) = kernelUpdater.update(A, B, noise, kernel)
  private def updateKernel(A: DenseMatrix[Double], b: DenseVector[Double]) = kernelUpdater.update(A, b, noise, kernel)
  private def updateKernel(a: DenseVector[Double], b: DenseVector[Double]) = kernelUpdater.update(a, b, noise, kernel)

  /**
    *  Build a new symmetric matrix from the given blocks.
    *    [  A  B ]
    *    [ B.t C ]
    */
  private def expandMatrix(A: DenseMatrix[Double], B: DenseMatrix[Double], C: DenseMatrix[Double]) = {
    require(A.rows == A.cols && C.rows == C.cols)
    require(B.rows == A.rows && B.cols == C.cols)

    val X = DenseMatrix.zeros[Double](A.rows + B.cols, A.cols + B.cols)

    X(0 until A.rows, 0 until A.cols) := A
    X(0 until A.rows, A.cols until X.cols) := B
    X(A.rows until X.rows, 0 until A.cols) := B.t
    X(A.rows until X.rows, A.cols until X.cols) := C
    X
  }

  // This should be Breeze, but there's no way to construct a Matrix from col vectors
  @inline
  private def makeColMatrix(vs: IndexedSeq[DenseVector[Double]]) = {
    val rm = DenseMatrix.zeros[Double](vs.head.length, vs.length)
    cforRange(0 until vs.length) { i =>
      rm(::, i) := vs(i)
    }
    rm
  }

  def update(xs: IndexedSeq[DenseVector[Double]], t: DenseVector[Double]): GaussianProcess = {
    val X = makeColMatrix(xs)

    val (m, cov, newX, newT) = if (model.isDefined) {
      val m = model.get
      val mean = DenseVector.fill[Double](m.X.cols + X.cols, this.mean)

      val K = updateKernel(m.X, X)
      val C = updateKernel(X, X)
      val cov = expandMatrix(m.cov, K, C)

      val newX = DenseMatrix.horzcat(m.X, X)
      val newT = DenseVector.vertcat(m.t, t)
      (mean, cov, newX, newT)
    } else {
      val mean = DenseVector.fill[Double](X.cols, this.mean)
      val cov = updateKernel(X, X)
      (mean, cov, X, t)
    }

    model = Some(GPModel(m, cov, newX, newT))
    this
  }

  def update(x: DenseVector[Double], t: Double): GaussianProcess = {
    update(Array(x), DenseVector(t))
    this
  }

  def predict(X: IndexedSeq[DenseVector[Double]]): Array[(Double, Double)] = {
    X.par.map { v =>
      predict(v)
    }.toArray
  }

  /**
    * Predicts a t for a given feature.
    * @param x A DenseVector containing the features of the given x.
    * @return A 2-tuple containing the mean and variance of the predicted t.
    */
  def predict(x: DenseVector[Double]) = {
    val m = model.get
    val invC = m.invC

    val k = updateKernel(m.X, x)
    val c = updateKernel(x, x)

    val tkiC: Transpose[DenseVector[Double]] = dsmv(invC, k).t
    val tMean = mean + (tkiC * m.tMinusMean)
    val tVar = c - (tkiC * k)

    (tMean, sqrt(tVar))
  }

  /**
    * Predicts a vector of t's for a given matrix of features.
    * @param X new features to predict a t for. X is assumed to be in col major order ()
    * @return A 2-tuple containing a DenseVector of means and DenseVector of variances of the predicted t's.
    */
  def predict(X: DenseMatrix[Double]) = {
    val m = model.get
    val invC = m.invC

    val K = updateKernel(m.X, X)

    val c: DenseVector[Double] = X(::, *).map { x => updateKernel(x, x) }.t

    val K2: DenseMatrix[Double] = dsymm(invC, K)
    val tMean = DenseVector.fill[Double](X.cols)(mean) + (m.tMinusMean.t * K2).t

    val tVar = DenseVector.zeros[Double](X.cols)
    cforRange(0 until X.cols) { i =>
      tVar(i) = c(i) - (K2(::, i) dot K(::, i))
    }

    (tMean, sqrt(tVar))
  }

  def nextBatch(lastBestT: Double, samples: Int = 10000000, blocks: Int = 10000): (Double, DenseVector[Double]) = {
    (0 until blocks).par.map { i =>
      var best: (Double, DenseVector[Double]) = (0.0, null)
      val X = DenseMatrix.rand[Double](7, samples / blocks)
      val t = predict(X)

      cforRange(0 until X.cols) { i =>
        val m = t._1(i)
        val v = t._2(i)
        val u = utility(m, v, lastBestT)
        if (u > best._1) best = (m, X(::, i))
      }
      best
    }.head

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

