package tomsopt.kernel

import breeze.linalg.DenseVector
import org.scalatest._
import org.scalatest.prop._
import tomsopt.kernel._
import org.scalacheck.Gen

class KernelSpec extends PropSpec with Matchers with PropertyChecks {

  val kernels =
    Table(
      ("kernel"),
      (new ARDKernel()),
      (new Matern52())
    )

  val vecs = for (n <- Gen.choose(0, 100)) yield (DenseVector.rand[Double](n), DenseVector.rand[Double](n))

  property("A kernel should produce the same value fo its native and java implementations") {
    forAll(kernels) { kernel =>
      forAll(vecs) { case (x1, x2) =>
        val kj = kernel.apply(x1.data, x1.offset, x2.data, x2.offset, x1.length)
        val kn = kernel.applyNative(x1.data, x1.offset, x2.data, x2.offset, x1.length)

        kj should === (kn +- 0.001)
      }
    }
  }

  property("A kernel should be >0") {
    forAll(kernels) { kernel =>
      forAll(vecs) { case (x1, x2) =>
        val kj = kernel.apply(x1.data, x1.offset, x2.data, x2.offset, x1.length)
        val kn = kernel.applyNative(x1.data, x1.offset, x2.data, x2.offset, x1.length)

        kj should be >= 0.0
        kn should be >= 0.0
      }
    }
  }

}
