package tomsopt.gp

import breeze.linalg._
import breeze.numerics._
import spire.syntax.cfor._
import tomsopt.kernel._
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

/**
  * A Gaussan Process Regression model. Based on formulation described in
  * Bishop - Patterns Recognition and Machine Learning, Chap 6.
  * @param kernel The model's kernel function.
  * @param noise The model's noise parameter.
  * @param mean The model's mean. The default value of 0.0 is probably sufficient.
  * @param useNative Whether to use JNI-Native routines to update the Kernel Matrix. Defatuls to true.
  */
class GaussianProcess(kernel: Kernel, noise: Double, mean: Double = 0.0, useNative: Boolean = true) {

  var model: Option[GPModel] = None

  private val kernelUpdater = if (useNative) new KernelUpdaterNative else new KernelUpdaterArray

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

  /**
    * Update the given GP model with the given observations.
    * @param xs A Seq of DenseVectors representing features points - 1
    * @param t A DenseVector of observed values for the given features
    */
  def update(xs: IndexedSeq[DenseVector[Double]], t: DenseVector[Double]): GaussianProcess = {
    val X = buildMatrixFromColVecs(xs)

    val (m, cov, newX, newT) = if (model.isDefined) {
      val m = model.get
      val mean = DenseVector.fill[Double](m.X.cols + X.cols, this.mean)

      // Work out parts missing from Cov and glue together a new matrix
      val K = updateKernel(m.X, X)
      val C = updateKernel(X, X)
      val cov = expandMatrix(m.cov, K, C)

      // Concatenate new observation onto existing ones
      val newX = DenseMatrix.horzcat(m.X, X)
      val newT = DenseVector.vertcat(m.t, t)
      (mean, cov, newX, newT)
    } else {
      // Easy, just set observations as current model
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

  /**
    * Predicts a t for a given feature.
    *
    * @param x A DenseVector containing the features of the given x.
    * @return A 2-tuple containing the mean and variance of the predicted t.
    */
  def predict(x: DenseVector[Double]): (Double, Double) = {
    val r = predict(x.toDenseMatrix)
    (r._1(0), r._2(0))
  }

  /**
    * Predicts a vector of t's for a given matrix of features.
    * For GP with parameters:
    *     $$ m = [ m_a, m_b ] $$ and
    *     $$
    *       \Sigma =
    *         \begin{bmatrix}
    *           C & K \\
    *           K^t & c
    *         \end{bmatrix}
    *     $$
    *   Where we already know $$ m_a, C $$, our observations so far, we can predict $$ m_b, K,c $$ by conditional
    *   our Gaussian on these.
    *     $$ m_{b|a} = m_b + K^t C^{-1} (t_b - m_a) $$
    *     $$ v_{b|a} = C_{b} - K^t C^1 K $$
    *
    * @param X new features to predict a t's for. X is assumed to be in col major order.
    * @return A 2-tuple containing DenseVector's of means and variances of the predicted t's.
    */
  def predict(X: DenseMatrix[Double]): (DenseVector[Double], DenseVector[Double]) = {
    val m = model.get
    val invC = m.invC

    val K = updateKernel(m.X, X)

    // We assume that each unknown, t, is independent of each other. It doesn't necessary have to be done this
    // way, but it makes the inference much more tractable.
    val c: DenseVector[Double] = X(::, *).map { x => updateKernel(x, x) }.t

    // Cache $$ K^ C^{-1} $$, since used across both equations. Also C is symmetric (so there so is C^-1), we make use
    // of this to avoid transposing matrix until in smaller dimensions (maybe not necessary should re-benchmark).
    val K2: DenseMatrix[Double] = dsymm(invC, K)
    val tMean = DenseVector.fill[Double](X.cols)(mean) + (m.tMinusMean.t * K2).t

    // Since we assume new observations are independent we can also assume that $$ K^t C^1 K $$ is diagonal. But rather
    // than computing full 3 matrix multiplications, to then only take the diagonal, we save ourselves some computations
    // and only compute diagonal entries.
    val tVar = DenseVector.zeros[Double](X.cols)
    cforRange(0 until X.cols) { i =>
      tVar(i) = c(i) - (K2(::, i) dot K(::, i))
    }

    (tMean, sqrt(tVar))
  }

}

