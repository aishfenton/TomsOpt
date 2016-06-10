package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}
import tomsopt.BayesOptimizer
import tomsopt.kernel._
import tomsopt.utility._
import breeze.linalg._

/**
  * @author Aish Fenton.
  */
class EndToEnd {

  def genX(n: Int) = (0 until n).map { i => DenseVector.rand[Double](7) }
  def func(x: DenseVector[Double]): Double = { math.sin(sum(x)) + ( randomDouble() * 0.01 ) }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def predictNative: Double = {
    val bayOpt = new BayesOptimizer(new ARDKernel(), new ExpectedImprovement(), 0.1, true)

    val obsX = genX(50)
    val obsY = DenseVector(obsX.map(func): _*)
    bayOpt.update(obsX, obsY)
    bayOpt.likelyMax()._1
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def predictJava: Double = {
    val bayOpt = new BayesOptimizer(new ARDKernel(), new ExpectedImprovement(), 0.1, false)

    val obsX = genX(50)
    val obsY = DenseVector(obsX.map(func): _*)
    bayOpt.update(obsX, obsY)
    bayOpt.likelyMax()._1
  }

}
