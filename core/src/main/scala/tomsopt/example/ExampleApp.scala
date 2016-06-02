package tomsopt.example

import breeze.linalg._
import tomsopt.kernel.BKernel
import tomsopt.utility.ExpectedImprovement
import tomsopt.GP

import vegas._
import vegas.render.WindowRenderer._

object ExampleApp extends App {

  def genX(n: Int) = DenseMatrix.rand[Double](n, 1).map(x => x * 2 * Math.PI )
  def func(x: Double) = { math.sin(x) + ( randomDouble() * 0.01 ) }

  val gp = new GP(new BKernel, new ExpectedImprovement, 0.1)

  val obsX = genX(50)
  val obsY = obsX(*, ::).map(x => func(x(0)))

  gp.update(obsX, obsY)

  val newX = genX(1000000)
  val newT = gp.predict(newX(*, ::).toIndexedSeq.map(_.t)).map(_._1)

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

