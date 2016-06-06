package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import breeze.linalg._
import spire.syntax.cfor._

/**
  * @author Aish Fenton.
  */
@State(Scope.Thread)
class LinAlg {

  val A = DenseMatrix.rand[Double](1000, 1000)

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def colKernel: DenseMatrix[Double] = {
    val B = A
    val g = DenseMatrix.zeros[Double](A.cols, B.cols)

    cforRange(0 until A.cols) { i =>
      val v = A(::, i)
      cforRange(0 until B.cols) { j =>
        val k = v dot B(::, j)
        g(i, j) = k
      }
    }
    g
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def rowKernel: DenseMatrix[Double] = {
    val tA = A.t
    val tB = A.t
    val g = DenseMatrix.zeros[Double](A.rows, tB.rows)
    cforRange(0 until A.rows) { i =>
      val v = tA(::, i)
      cforRange(0 until tB.cols) { j =>
        val k = v dot tB(::, j)
        g(i, j) = k
      }
    }
    g
  }

  // I'm assuming there is some cost to many v * M multplications, as opposed to going a single
  // large M * M. Assumed costs are a) small JNI overhead, b) further BLAS optimizations on M*M
  //
  // Answer:

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def bulkMxM: DenseMatrix[Double] = {
//    val A = DenseMatrix.rand[Double](10000, 10000)
    val M = A.t * A
    M
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def loopVxM: Array[DenseVector[Double]] = {
    val tA =  A.t

    val r = new Array[DenseVector[Double]](A.rows)
    cforRange(0 until A.rows) { i =>
      val v = A(::, i)
      r(i) = DenseMatrix.implOpMulMatrix_DMD_DVD_eq_DVD( tA, v )
    }

    r
  }

  // Testing if row / col major makes any differences. Appears that it doesn't.

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def rowMajorMult: DenseMatrix[Double] = {
    A * A.t
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def colMajorMult: DenseMatrix[Double] = {
    A.t * A
  }
}
