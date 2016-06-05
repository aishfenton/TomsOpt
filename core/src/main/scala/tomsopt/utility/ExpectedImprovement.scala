package tomsopt.utility

import breeze.linalg.randomDouble
import breeze.stats.distributions.Gaussian

/**
  * @author Aish Fenton.
  */
class ExpectedImprovement extends Utility {

  @inline
  def apply(mean: Double, variance: Double, tBest: Double): Double = {
    val dist = Gaussian(mean, variance)
    ((mean - tBest) * dist.cdf(tBest)) + (variance * dist.pdf(tBest))
  }

}
