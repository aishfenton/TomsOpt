package tomsopt

import breeze.linalg._
import breeze.numerics._
import spire.syntax.cfor._
import breeze.stats.distributions.Gaussian
import tomsopt.kernel.Kernel
import tomsopt.utility.Utility

case class GPModel(mean: DenseVector[Double], cov: DenseMatrix[Double], X: DenseMatrix[Double], t: DenseVector[Double])

class GP(kernel: Kernel, activation: Utility, noise: Double, mean: Double = 0.0) {

  var model: Option[GPModel] = None

  /**
    * Calc cholesky decomposition with a jittered retry. Jitter starts off as 1e-6 mean of diag.
    * Shamelessly stolen from:
    *   https://github.com/lawrennd/ndlutil/blob/master/matlab/jitChol.m
    */
  @inline
  private def jChol(A: DenseMatrix[Double], maxRetries: Int = 10) = {
    def run(X: DenseMatrix[Double], jitter: Double, retries: Int): DenseMatrix[Double] = {
      try {
        val jM = DenseMatrix.eye[Double](X.rows) *= jitter
        cholesky(X + jM)
      } catch {
        case e:NotConvergedException => {
          if (retries < maxRetries) {
            println(s"cholesky retry ${retries + 1}")
            run(X, jitter * 10, retries + 1)
          } else {
            throw e
          }
        }
      }
    }

    require(A.rows == A.cols)
    val j = (diag(A).sum / A.rows).abs * 1e-6
    run(A, j, 0)
  }

  // Kernel wih noise
  @inline
  private def nKernel(x1: DenseVector[Double], x2: DenseVector[Double]) = kernel(x1, x2) + noise

  /**
    * Calculates the kernel between all row/col pairs of matrices Ai kdot Bj. Both matrices are assumed
    * to be in the right orientation when given (i.e. if B is additional rows of A, then it is assumed it already
    * transposed).
    *
    * @return A kdot B
    * TODO Breeze is in col-major order, so need to swap around
    */
  private def kernelProduct(A: DenseMatrix[Double], B: DenseMatrix[Double]) = {
    val g = DenseMatrix.zeros[Double](A.rows, B.cols)
    cforRange(0 until A.rows) { i =>
      cforRange(0 until B.cols) { j =>
        val k = nKernel(A(i, ::).t, B(::, j))
        g(i, j) = k
      }
    }
    g
  }

  // Save some computation, and annoyingly need to ensure that same noise is
  // applied to both i,j and j,i
  private def kernelProductSym(A: DenseMatrix[Double]) = {
    val tA = A.t

    val g = DenseMatrix.zeros[Double](A.rows, tA.cols)
    cforRange(0 until A.rows) { i =>
      cforRange(i until tA.cols) { j =>
        val k = nKernel(A(i, ::).t, tA(::, j))
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

  def update(X: DenseMatrix[Double], t: DenseVector[Double]): GP = {
    val (m, cov, newX, newT) = if (model.isDefined) {
      val existing = model.get
      val m = DenseVector.fill[Double](existing.X.rows + X.rows, mean)

      val K = kernelProduct(existing.X, X.t)
      val C = kernelProductSym(X)
      val cov = buildMatrix(existing.cov, K, C)

      val newX = DenseMatrix.vertcat(existing.X, X)
      val newT = DenseVector.vertcat(existing.t, t)
      (m, cov, newX, newT)
    } else {
      val m = DenseVector.fill[Double](X.rows, mean)
      val cov = kernelProductSym(X)
      (m, cov, X, t)
    }

    model = Some(GPModel(m, cov, newX, newT))
    this
  }

  def update(x: DenseVector[Double], t: Double): GP = {
    update(x.toDenseMatrix, DenseVector(t))
    this
  }

  @inline
  private def biLinearMult(x: Transpose[DenseVector[Double]], A: DenseMatrix[Double], y: DenseVector[Double]) = {
    val tx = x.t
    require(tx.length == y.length && y.length == A.rows && A.rows == A.cols, "must multiply same sizes")

    var r = 0.0
    cforRange(0 until tx.length) { i =>
      cforRange(0 until tx.length) { j =>
        r += A(i, j) * tx(i) * y(j)
      }
    }
    r
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

    val m = model.get
    // Yes I know, should'nt be doing inv, but Breeze doesn't have a triangle solver, and is it really that bad?
    // See: http://arxiv.org/abs/1201.6035v1
    val invL = inv(jChol(m.cov))
    val invC =  invL.t * invL

//    val results = Array.fill[(Double, Double)](X.rows)(null)

    val results = X.par.map { v =>
      val k = DenseVector.rand(m.X.rows) // kernelProduct(m.X, v.toDenseMatrix).toDenseVector
      val c = kernel(v, v)

//      val tMean = mean + 0.0 // biLinearMult(k.t, invC,  (m.t - m.mean))
//      val tVar = c - 0.0 // biLinearMult(k.t, invC, k)
      val tMean = mean + (k.t * invC * (m.t - m.mean) )
      val tVar = c - (k.t * invC * k)
      (tMean, sqrt(tVar))
    }

    println(X.size)
    results.toArray
  }

  def search(x: DenseVector[Double]) = {

  }

}

