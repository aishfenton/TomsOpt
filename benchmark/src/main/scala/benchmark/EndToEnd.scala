package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}
import tomsopt.GP
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
  def predict: Double = {

    def genX(n: Int) = DenseMatrix.rand[Double](n, 1).map(x => x * 2 * Math.PI )
    def func(x: Double) = { math.sin(x) + ( randomDouble() * 0.01 ) }

    val gp = new GP(new BKernel, new ExpectedImprovement, 0.1)

    val obsX = genX(50)
    val obsY = obsX(*, ::).map(x => func(x(0)))

    gp.update(obsX, obsY)

    val newX = genX(10000000)
    println(newX.rows)
    val newT = gp.predict(newX(*, ::).iterator.toIndexedSeq)
    newT.head._1
  }

}
