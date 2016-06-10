package tomsopt

import breeze.linalg._
import spire.syntax.cfor._

/**
  * @author Aish Fenton.
  */
package object math {

  lazy val blas = com.github.fommil.netlib.BLAS.getInstance
  lazy val lapack = com.github.fommil.netlib.LAPACK.getInstance

  /**
    * Calc cholesky decomposition with a jittered retry. Jitter starts off as 1e-6 mean of diag.
    * Shamelessly stolen from:
    *   https://github.com/lawrennd/ndlutil/blob/master/matlab/jitChol.m
    */
  def jChol(A: DenseMatrix[Double], maxRetries: Int = 10) = {
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

    require(A.rows == A.cols, "Matrix should be square")
    val j = (sum(diag(A)) / A.rows).abs * 1e-6
    run(A, j, 0)
  }

  def dsmv(A: DenseMatrix[Double], x: DenseVector[Double]) = {
    val rv = DenseVector.zeros[Double](x.length)

    blas.dsymv("u",
      A.rows, 1.0, A.data, A.offset, A.majorStride,
      x.data, x.offset, 1,
      0.0, rv.data, 0, 1
    )
    rv
  }

  def dsymm(A: DenseMatrix[Double], B: DenseMatrix[Double]) = {
    val C = DenseMatrix.zeros[Double](A.rows, B.cols)

    blas.dsymm("l","u",
      C.rows, C.cols,
      1.0, A.data, A.offset, A.majorStride,
      B.data, B.offset, B.majorStride,
      0.0, C.data, 0, C.majorStride
    )
    C
  }

  /**
    * Constructs a matrix from the given Column vectors.
    * TODO This should be Breeze, but there's no built-in way to construct a Matrix from col vectors, issue filed
    * here: https://github.com/scalanlp/breeze/issues/550
    */
  @inline
  def buildMatrixFromColVecs(vs: IndexedSeq[DenseVector[Double]]) = {
    val rm = DenseMatrix.zeros[Double](vs.head.length, vs.length)
    cforRange(0 until vs.length) { i =>
      rm(::, i) := vs(i)
    }
    rm
  }


}
