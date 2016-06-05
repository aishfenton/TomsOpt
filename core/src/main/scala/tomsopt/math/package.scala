package tomsopt

import breeze.linalg._

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
  @inline
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

    require(A.rows == A.cols)
    val j = (diag(A).sum / A.rows).abs * 1e-6
    run(A, j, 0)
  }

  @inline
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


}
