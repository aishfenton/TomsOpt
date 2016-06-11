package tomsopt

import tomsopt.gp.GaussianProcess
import utility._
import kernel._
import spire.syntax.cfor._
import spire.syntax.literals.si._
import breeze.linalg._

/**
  * Optimizes a given black-box function in a somewhat smart way that minimizes the amount of points evaluated. Underlying the
  * magic is a Gaussian Process Regression model.
  */
class BayesOptimizer(kernel: Kernel, utility: Utility, noise: Double, useNative: Boolean = true) {

  private val FractionPerThread = 0.001
  private val DefaultSamples = i"10 000 000"

  val gp = new GaussianProcess(kernel, noise, 0.0, useNative)

  /**
    * Update the Bayesian optimizer with new observation.
    * @param x DenseVector representing the observed point (i.e. the features)
    * @param y The observed value for the given point.
    */
  def update(x: DenseVector[Double], y: Double) = { gp.update(x, y); this }

  /**
    * Same as vector version but instead takes a batch of new observations.
    */
  def update(X: IndexedSeq[DenseVector[Double]], y: DenseVector[Double]) = { gp.update(X, y); this }

  /**
    * TODO this is just a place holder and would need to be more fully built out to include definitions and ranges of
    * features.
    */
  @inline
  private def sampleFeatures(n: Int) = {
    // XXX Erk hardcoded until something better is done.
    val Dim = 7
    DenseMatrix.rand[Double](Dim, n)
  }

  /**
    * Returns the next point that is most likely to maximize the utility function
    *
    * @param currentMax The known point that is currently maximal. Defaults to Double.MinValue, so can be ignore
    *                   when starting off.
    * @param accuracy A fraction that controls how thoroughly to explore (must be 0 < x < \inf). Defaults to 1.0,
    *                 which samples 10,000,000 in ~6secs.
    */
  def likelyMax(currentMax: Double = Double.MinValue, accuracy: Double = 1.0): (Double, DenseVector[Double]) = {
    val samples = (DefaultSamples * accuracy).toInt
    val blocks = (samples * FractionPerThread).toInt

    val blockMaxes = (0 until blocks).par.map { i =>
      // Tuple for best found in block, containing max value, and max point
      var nextMax: (Double, DenseVector[Double]) = (0.0, null)

      val X = sampleFeatures(samples / blocks)
      val t = gp.predict(X)

      // Find max within current block
      cforRange(0 until X.cols) { i =>
        val mean = t._1(i)
        val vari = t._2(i)
        val u = utility(mean, vari, currentMax)
        if (u > nextMax._1) nextMax = (mean, X(::, i))
      }
      nextMax
    }.toList

    // Return max among all blocks
    blockMaxes.sortBy(-1 * _._1).head

  }


}
