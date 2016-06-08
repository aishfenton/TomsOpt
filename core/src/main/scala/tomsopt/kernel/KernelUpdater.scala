package tomsopt.kernel

import breeze.linalg.{DenseVector, DenseMatrix}

/**
  * @author Aish Fenton.
  */
trait KernelUpdater {

  def update(a: DenseVector[Double], b: DenseVector[Double], noise: Double, kernel: Kernel) = {
    val ad = a.data
    val aOff = a.offset
    val bd = b.data
    val bOff = b.offset
    val dim = a.length
    kernel(ad, aOff, bd, bOff, dim) + noise
  }

  def update(A: DenseMatrix[Double], b: DenseVector[Double], noise: Double, kernel: Kernel): DenseVector[Double]
  def update(A: DenseMatrix[Double], B: DenseMatrix[Double], noise: Double, kernel: Kernel): DenseMatrix[Double]
}

