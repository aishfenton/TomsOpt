package tomsopt.kernel

import breeze.linalg._
import breeze.numerics._
import spire.syntax.cfor._

/**
  * @author Aish Fenton.
  */
// ARD Kernel from Bishop's Pattern Recognition Eqn. 6.63.
class ARDKernel(param: Array[Double] = Array(0.1, 0.1, 0.1, 0.1)) extends Kernel {

  val param1 = param(0)
  val param2 = param(1)
  val param3 = param(2)
  val param4 = param(3)

  val supportsNative = true;

  @inline
  def apply(x1: Array[Double], x1Offset: Int, x2: Array[Double], x2Offset: Int, dim: Int): Double = {
    var sdist = 0.0
    var dot = 0.0

    cforRange(0 until dim) { i =>
      val a = x1(x1Offset + i)
      val b = x2(x2Offset + i)
      sdist += pow(a - b, 2)
      dot += a * b
    }
    (param1 * exp(-0.5 * param2 * sdist)) + param3 + (param4 * dot)
  }

}
