package tomsopt.kernel

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest._
import org.scalatest.prop._
import tomsopt.kernel._
import org.scalacheck.Gen
import breeze.linalg._

class KernelUpdaterSpec extends PropSpec with Matchers with PropertyChecks {

  val kuArray = new KernelUpdaterArray
  val kuNative = new KernelUpdaterNative

  val kernels =
    Table(
      ("kernel"),
      (new ARDKernel()),
      (new Matern52())
    )

  val matrices = for {
    obsN <- Gen.choose(0, 100)
    newN <- Gen.choose(0, 100)
    dimN <- Gen.choose(1, 50)
  } yield (DenseMatrix.rand[Double](dimN, obsN), DenseMatrix.rand[Double](dimN, newN))

  property("The native and java updater should produce the same covariance matrix ") {
    forAll(kernels) { kernel =>
      forAll(matrices) { case (x, k) =>

        val jK = kuArray.update(x, k, 1.1, kernel)
        val nK = kuNative.update(x, k, 1.1, kernel)

        jK.toArray.zip(nK.toArray).foreach { case (x1, x2) => x1 should === (x2 +- 0.001) }
      }
    }
  }



}
