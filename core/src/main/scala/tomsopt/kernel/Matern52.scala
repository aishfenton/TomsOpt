package tomsopt.kernel

import breeze.linalg.DenseVector
import ch.jodersky.jni.nativeLoader
import spire.syntax.cfor._
import breeze.numerics._
/**
  * Matern52 Kernel.
  * TODO needs to support array of legnth scale parameters.
  */
@nativeLoader("TomsOpt0")
class Matern52(signalVariance: Double = 0.1, lengthScales: Double = 0.1) extends Kernel {

  val Sqrt5 = sqrt(5)
  val D53 = 5.0 / 3.0

  val supportsNative = true;

  @inline
  def apply(x1: Array[Double], x1Offset: Int, x2: Array[Double], x2Offset: Int, dim: Int): Double = {

    var r2 = 0.0
    cforRange(0 until dim) { i =>
      val a = x1(x1Offset + i)
      val b = x2(x2Offset + i)
      r2 += pow(a - b, 2) * lengthScales
    }
    val r = sqrt(r2)

    signalVariance * (1 + (Sqrt5 * r) + (D53 * r2)) * exp(-(Sqrt5 * r))

  }

  @native
  def applyNative(x1: Array[Double], x1Offset: Int, x2: Array[Double], x2Offset: Int, dim: Int): Double;

}
