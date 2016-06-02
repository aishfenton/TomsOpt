package tomsopt.kernel

import breeze.linalg._

/**
  * @author Aish Fenton.
  */
trait Kernel {
  def apply(x1: DenseVector[Double], x2: DenseVector[Double]): Double
}
