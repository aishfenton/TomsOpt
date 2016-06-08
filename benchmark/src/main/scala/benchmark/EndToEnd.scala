package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}
import tomsopt.GaussianProcess
import tomsopt.kernel._
import tomsopt.utility._
import breeze.linalg._

/**
  * @author Aish Fenton.
  */
class EndToEnd {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def batchNext: Double = {

    def genX(n: Int) = (0 until n).map { i => DenseVector.rand[Double](7) }
    def func(x: DenseVector[Double]): Double = { math.sin(sum(x)) + ( randomDouble() * 0.01 ) }

//    val gp = new GaussianProcess(new Matern52(0.1, DenseVector.ones[Double](7).toArray), new ExpectedImprovement, 0.1)
    val gp = new GaussianProcess(new ARDKernel(), new ExpectedImprovement(), 0.1, 0.0, new KernelUpdaterNative)

    val obsX = genX(50)
    val obsY = DenseVector(obsX.map(func): _*)

    gp.update(obsX, obsY)

    val newX = gp.nextBatch(-1)
    newX._1
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def loopNext: Double = {

    def genX(n: Int) = (0 until n).map { i => DenseVector.rand[Double](7) }
    def func(x: DenseVector[Double]): Double = { math.sin(sum(x)) + ( randomDouble() * 0.01 ) }

    val gp = new GaussianProcess(new ARDKernel, new ExpectedImprovement, 0.1, 0.0, new KernelUpdaterNative)

    val obsX = genX(50)
    val obsY = DenseVector(obsX.map(func): _*)

    gp.update(obsX, obsY)

    val newX = gp.next(-1)
    newX._1
  }

}
