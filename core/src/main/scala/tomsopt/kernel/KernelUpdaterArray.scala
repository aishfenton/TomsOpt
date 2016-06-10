package tomsopt.kernel

import breeze.linalg.{DenseVector, DenseMatrix}
import spire.syntax.cfor._

/**
  * @author Aish Fenton.
  */
class KernelUpdaterArray extends KernelUpdater {

  def update(A: DenseMatrix[Double], B: DenseMatrix[Double], noise: Double, kernel: Kernel) = {
    require(A.rows == B.rows && B.rows > 0, "A and B matrices can't have different number of rows, and both must have > 0 rows")

    val C = DenseMatrix.zeros[Double](A.cols, B.cols)
    val ad = A.data
    val aOffset = A.offset
    val bd = B.data
    val bOffset = B.offset
    val al = A.cols
    val bl = B.cols
    val dim = A.rows

    cforRange(0 until bl) { i =>
      val bOff = (i * dim) + bOffset
      cforRange(0 until al) { j =>
        val aOff = (j * dim) + aOffset
        val k = kernel(bd, bOff, ad, aOff, dim) + noise
        C(j, i) = k
      }
    }
    C
  }

  def update(A: DenseMatrix[Double], b: DenseVector[Double], noise : Double, kernel: Kernel) = {
    require(A.rows == b.length && b.length > 0, "A and b can't be different sizes (i.e rows and length) and must be > 0")

    val c = DenseVector.zeros[Double](A.cols)
    val ad = A.data
    val aOffset = A.offset
    val al = A.cols
    val bd = b.data
    val bOff = b.offset
    val dim = b.length

    cforRange(0 until al) { i =>
      val aOff = (i * dim) + aOffset
      val k = kernel(ad, aOff, bd, bOff, dim) + noise
      c(i) = k
    }
    c
  }

}
