package tomsopt.kernel

import breeze.linalg._
import breeze.numerics._
import spire.syntax.cfor._

/**
  * @author Aish Fenton.
  */
// Kernel from Bishop's Pattern Recognition Eqn. 6.63.
class BKernel(param: Array[Double] = Array(0.1, 0.1, 0.1, 0.1)) extends Kernel {

  val param1 = param(0)
  val param2 = param(1)
  val param3 = param(2)
  val param4 = param(3)

  @inline
  def apply(x1: DenseVector[Double], x2: DenseVector[Double]): Double = {
    var sdist = 0.0
    var dot = 0.0

    zipValues(x1, x2).foreach { (a, b) =>
      sdist += pow(a - b, 2)
      dot += a * b
    }
    (param1 * exp(-0.5 * param2 * sdist)) + param3 + (param4 * dot)
  }

}
