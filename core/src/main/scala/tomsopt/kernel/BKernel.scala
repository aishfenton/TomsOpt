package tomsopt.kernel

import breeze.linalg._
import breeze.numerics._

/**
  * @author Aish Fenton.
  */
// Kernel from Bishop's Pattern Recognition Eqn. 6.63.
class BKernel(param: Array[Double] = Array(0.1, 0.1, 0.1, 0.1)) extends Kernel {

  // exponential = self.thetas[0] * exp( -0.5 * self.thetas[1] * sum( (x - y)**2 ) )
  // linear = self.thetas[3] * dot(x,y)
  // constant = self.thetas[2]
  // return exponential + constant + linear
  @inline
  def apply(x1: DenseVector[Double], x2: DenseVector[Double]): Double = {
    var sdist = 0.0
    var dot = 0.0
    zipValues(x1, x2).foreach { (a, b) =>
      sdist += pow(a - b, 2)
      dot += a * b
    }
    (param(0) * exp(-0.5 * param(1) * sdist)) + param(2) + (param(3) * dot)
  }

}
