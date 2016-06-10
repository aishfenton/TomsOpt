package tomsopt.kernel

import breeze.linalg.{DenseVector, DenseMatrix}
import ch.jodersky.jni.nativeLoader

/**
  * @author Aish Fenton.
  */
@nativeLoader("TomsOpt0")
class KernelUpdaterNative extends KernelUpdater {

  /**
    * Calculates kernel matrix for A and B and places into C, using the column vectors of both A and B.
    * @param A DenseMatrix assumed to be in column major
    * @param B DenseMatrix assumed to be in column major (and not transposed, kernel is taken between cols of A and B)
    * @param kernel The Kernel class used to do the update
    */
  def update(A: DenseMatrix[Double], B: DenseMatrix[Double], noise: Double, kernel: Kernel) = {
    require(A.rows == B.rows && B.rows > 0, "A and B matrices can't have different number of rows, and both must have > 0 rows")
    require(kernel.supportsNative, s"Kernel ${kernel.getClass.getName} doesn't support native")

    val C = DenseMatrix.zeros[Double](A.cols, B.cols)
    val result = _nativeUpdate(
      A.data, A.offset, A.size,
      B.data, B.offset, B.size,
      C.data, C.offset, C.size,
      A.rows, noise, kernel.getClass.getName)

    if (result != 0) throw new Exception("Native update failed for some reason? Check stderr")

    C
  }

  /**
    * Calculates kernel matrix for A and b and places into c, using the column vectors of A.
    * @param A DenseMatrix assumed to be in column major
    * @param b DenseVector
    * @param kernel The Kernel class used to do the update
    */
  def update(A: DenseMatrix[Double], b: DenseVector[Double], noise: Double, kernel: Kernel) = {
    require(A.rows == b.length && b.length > 0, "A and b can't be different sizes (i.e rows and length) and must be > 0")
    require(kernel.supportsNative, s"Kernel ${kernel.getClass.getName} doesn't support native")

    val c = DenseVector.zeros[Double](A.cols)
    val result = _nativeUpdate(
      A.data, A.offset, A.size,
      b.data, b.offset, b.length,
      c.data, c.offset, c.length,
      A.rows, noise, kernel.getClass.getName)

    if (result != 0) throw new Exception("Native update failed for some reason? Check stderr")
    c
  }

  @native
  private def _nativeUpdate(A: Array[Double], aOffset: Int, aLength: Int,
                    B: Array[Double], bOffset: Int, bLength: Int,
                    C: Array[Double], cOffset: Int, cLength: Int,
                    dim: Int, noise: Double, kernel: String): Int = ???

}
