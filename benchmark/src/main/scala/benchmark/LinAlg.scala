package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}
import breeze.linalg._

/**
  * @author Aish Fenton.
  */
class LinAlg {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def RowMajorMult: Double = {
    val A = DenseMatrix.rand(10000,10000)
    val M = A * A.t

    M(0,0)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def ColMajorMult: Double = {
    val A = DenseMatrix.rand(10000,10000)
    val M = A.t * A

    M(0,0)
  }
}
