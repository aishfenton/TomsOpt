package tomsopt.gp

import breeze.linalg._
import breeze.numerics._
import org.scalatest._
import org.scalatest.prop._
import tomsopt.kernel.{ARDKernel, Matern52}
import tomsopt.math._

class GaussianProcessSpec extends PropSpec with Matchers with PropertyChecks {

  val funcs = Map(
    "sin" -> { (x:Double) => sin(x) },
    "x^2" -> ( (x:Double) => pow(x,2) + x )
  )

  def genData(n: Int, func: String) = {
    def genX(n: Int) = (0 until n).map { i => DenseVector.rand[Double](1).map(_ * 10) }
    def genY(x: DenseVector[Double]): Double = { funcs(func)(sum(x)) + ( randomDouble() * 0.01 ) }

    val obsX = genX(n)
    val obsY = DenseVector(obsX.map(genY): _*)

    val splitPoint = n / 2
    val (trainX, testX) = obsX.splitAt(splitPoint)
    val (trainY, testY) = (obsY(0 until splitPoint), obsY(splitPoint until n))

    (trainX, trainY, testX, testY)
  }

  val examples =
    Table(
      ("n", "max-off-points", "native", "kernel", "func"),
      (500, 10, false, new ARDKernel(), "sin"),
      (50,  5, true,  new ARDKernel(), "sin"),
      (500, 10, false, new Matern52(),  "sin"),
      (50,  5, true,  new Matern52(),  "sin"),
      (500, 10, false, new ARDKernel(), "x^2"),
      (50,  5, true,  new ARDKernel(), "x^2"),
      (500, 10, false, new Matern52(),  "x^2"),
      (50,  5, true,  new Matern52(),  "x^2")
    )

  property("An GP should fit a simple model") {
    forAll(examples) { case (n, maxOffPoints, native, kernel, func) =>

      val gp = new GaussianProcess(kernel, 0.1, 0.0, native)
      val (trainX, trainY, testX, testY) = genData(n, func)

      gp.update(trainX, trainY)
      val predictedY = gp.predict(buildMatrixFromColVecs( testX ))._1

      // We define "fit" to mean most (<= maxOffPoints) of the predicted points should be within 0.1 of observed
      predictedY.toArray.zip(testY.toArray).filter { case (a,b) => (a - b) > 0.1 }.size shouldBe <= (maxOffPoints)

    }
  }

}
