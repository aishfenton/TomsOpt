package tomsopt.example

import breeze.linalg._
import tomsopt.gp.GaussianProcess
import tomsopt.kernel._
import tomsopt.utility.ExpectedImprovement
import tomsopt.math._
import vegas._
import vegas.render.WindowRenderer._

object ExampleApp extends App {

  def genX(n: Int) = (0 until n).map { i => DenseVector.rand[Double](1).map(_ * Math.PI * 2) }
  def func(x: DenseVector[Double]): Double = { math.sin(sum(x)) + ( randomDouble() * 0.01 ) }

  val gp = new GaussianProcess(new ARDKernel(), 0.1, 0.0, true)

  val obsX = genX(50)
  val obsY = DenseVector(obsX.map(func): _*)

  gp.update(obsX, obsY)
  val newX = genX(50)
  val newY = gp.predict(buildMatrixFromColVecs( newX ))._1

  def toData(x: IndexedSeq[DenseVector[Double]], y:DenseVector[Double], typ: String) = x.map(c => sum(c))
    .toArray.zip(y.toArray).map { case(x,y) => Map("x" -> x, "y" -> y, "type" -> typ ) }

  Vegas()
    .withData(toData(newX, newY, "predicted") ++ toData(obsX, obsY, "observed"))
    .encodeX("x", Quantitative)
    .encodeY("y", Quantitative)
    .encodeColor("type", Nominal)
    .encodeShape("type", Nominal)
    .mark(Line)
    .show

  Thread.sleep(35000)

}

