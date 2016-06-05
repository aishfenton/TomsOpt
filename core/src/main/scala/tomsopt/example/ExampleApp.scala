package tomsopt.example

import breeze.linalg._
import tomsopt.kernel.BKernel
import tomsopt.utility.ExpectedImprovement
import tomsopt.GP

import vegas._
import vegas.render.WindowRenderer._

object ExampleApp extends App {

  def genX(n: Int) = (0 until n).map { i => DenseVector.rand[Double](7) }
  def func(x: DenseVector[Double]): Double = { math.sin(sum(x)) + ( randomDouble() * 0.01 ) }

  val gp = new GP(new BKernel, new ExpectedImprovement, 0.1)

  val obsX = genX(50)
  val obsY = DenseVector(obsX.map(func): _*)

  gp.update(obsX, obsY)

//  val newX = genX(1000000)
//  val newT = gp.predict(newX).map(_._1)
  val r = gp.nextBatch(-1)

  println("Plotting!")
//  Vegas()
//    .withDataXY( obsX.toArray.zip(obsY.toArray) )
//    .encodeX("x", Quantitative)
//    .encodeY("y", Quantitative)
//    .mark(Square)
//    .show
//
//  Vegas()
//    .withDataXY( newX.toArray.zip(newT) )
//    .encodeX("x", Quantitative)
//    .encodeY("y", Quantitative)
//    .mark(Circle)
//    .show

//  Thread.sleep(35000)

}

