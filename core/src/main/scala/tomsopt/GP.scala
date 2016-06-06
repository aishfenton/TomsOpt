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
    * to be in the same orientation when given (i.e. if B is additional cols of A, then it is assumed already
    * both A and B are cols).
    *
    * @return A kdot B
    */
  @inline
  private def kernelProduct(A: DenseMatrix[Double], B: DenseMatrix[Double]) = {
    val g = DenseMatrix.zeros[Double](A.cols, B.cols)
    val al = A.cols
    val bl = B.cols
    val tmp = A(::, *).toIndexedSeq.toArray
    cforRange(0 until bl) { i =>
      val v = B(::, i)
      cforRange(0 until al) { j =>
        val k = nKernel(v, tmp(j))
        g(j, i) = k
      }
    }
    g
  }

  @inline
  private def kernelProduct(A: DenseMatrix[Double], v: DenseVector[Double]) = {
    val g = DenseVector.zeros[Double](A.cols)
    val al = A.cols
    cforRange(0 until al) { i =>
      val k = nKernel(A(::, i), v)
      g(i) = k
    }
    g
  }

  // Save some computation, and annoyingly need to ensure that same noise is
  // applied to both i,j and j,i.
  // Assumed A in col major format.
  @inline
  private def kernelProductSym(A: DenseMatrix[Double]) = {
    val g = DenseMatrix.zeros[Double](A.cols, A.cols)
    cforRange(0 until A.cols) { i =>
      cforRange(i until A.cols) { j =>
        val k = nKernel(A(::, i), A(::, j))
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

  // This should be Breeze, but there's no way to contrust a Matrix from col vectors
  @inline
  private def makeColMatrix(vs: IndexedSeq[DenseVector[Double]]) = {
    val rm = DenseMatrix.zeros[Double](vs.head.length, vs.length)
    cforRange(0 until vs.length) { i =>
      rm(::, i) := vs(i)
    }
    rm
  }

  def update(xs: IndexedSeq[DenseVector[Double]], t: DenseVector[Double]): GP = {
    val X = makeColMatrix(xs)

    val (m, cov, newX, newT) = if (model.isDefined) {
      val m = model.get
      val mean = DenseVector.fill[Double](m.X.cols + X.cols, this.mean)

      val K = kernelProduct(m.X, X)
      val C = kernelProductSym(X)
      val cov = expandMatrix(m.cov, K, C)

      val newX = DenseMatrix.horzcat(m.X, X)
      val newT = DenseVector.vertcat(m.t, t)
      (mean, cov, newX, newT)
    } else {
      val mean = DenseVector.fill[Double](X.cols, this.mean)
      val cov = kernelProductSym(X)
      (mean, cov, X, t)
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

  // Assumed to be col major order (i.e. each feature vector x, is a column)
  def predictBatch(X: DenseMatrix[Double]) = {
    val m = model.get
    val invC = m.invC

    val K = kernelProduct(m.X, X)
    val c: DenseVector[Double] = X(::, *).map { x => nKernel(x, x) }.t

    val K2: DenseMatrix[Double] = dsymm(invC, K)
    val tMean = DenseVector.fill[Double](X.cols)(mean) + (m.meanAdjustedT.t * K2).t

    val tVar = DenseVector.zeros[Double](X.cols)
    cforRange(0 until X.cols) { i =>
      tVar(i) = c(i) - (K2(::, i) dot K(::, i))
    }

    (tMean, sqrt(tVar))
  }

  def nextBatch(lastBestT: Double, samples: Int = 10000000, blocks: Int = 1000): (Double, DenseVector[Double]) = {
    (0 until blocks).par.map { i =>
      var best: (Double, DenseVector[Double]) = (0.0, null)
      val X = DenseMatrix.rand[Double](7, samples / blocks)
      val t = predictBatch(X)

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

