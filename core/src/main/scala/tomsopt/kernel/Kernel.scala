package tomsopt.kernel

import breeze.linalg._

/**
  * @author Aish Fenton.
  */
trait Kernel {
  def apply(x1: Array[Double], x1Offset: Int, x2: Array[Double], x2Offset: Int, dim: Int): Double
  def applyNative(x1: Array[Double], x1Offset: Int, x2: Array[Double], x2Offset: Int, dim: Int): Double;
  def supportsNative: Boolean
}
