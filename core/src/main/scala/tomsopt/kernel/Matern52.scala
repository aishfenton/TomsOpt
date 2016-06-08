package tomsopt.kernel

import breeze.linalg.DenseVector
import spire.syntax.cfor._
import breeze.numerics._
/**
  * @author Aish Fenton.
  */
class Matern52(signalVariance: Double, lengthScales: Array[Double]) extends Kernel {

  val sqrt5 = sqrt(5)
  val d53 = 5.0 / 3.0

  val supportsNative = true;

  @inline
  def apply(x1: Array[Double], x1Offset: Int, x2: Array[Double], x2Offset: Int, dim: Int): Double = {

    var r2 = 0.0
    cforRange(0 until dim) { i =>
      val a = x1(x1Offset + i)
      val b = x2(x2Offset + i)
      r2 += pow(a - b, 2) * lengthScales(i)
    }
    val r = sqrt(r2)

    signalVariance * (1 + (sqrt5 * r) + (d53 * r2)) * exp(-(sqrt5 * r))

  }

}
