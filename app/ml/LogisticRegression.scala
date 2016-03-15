package ml

import breeze.linalg.{sum, DenseMatrix, DenseVector}
import breeze.numerics.{log, sigmoid, exp}
import breeze.optimize.{LBFGS, DiffFunction}

/**
 * Created by shashwat on 3/13/16.
 */
object LogisticRegression {

  def hypothesis(X: DenseMatrix[Double], theta: DenseVector[Double]): DenseVector[Double] = {
    sigmoid(X * theta)
  }

  def computeCost(X: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double], lambda: Double): Double = {
    val m = X.rows
    val n = X.cols
    val onesVector = DenseVector.ones[Double](y.length)
    val hX = hypothesis(X, theta)
    (-1 * sum((y :* log(hX)) + ((onesVector - y) :* log(onesVector - hX))) / m) +
      (lambda / (2*m) * sum(theta.slice(1,theta.length)))
  }

  def costFunction(X: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double], lambda: Double): (Double, DenseVector[Double]) = {
    val m = X.rows
    val n = X.cols
    val cost = computeCost(X, y, theta, lambda)
    val hX = hypothesis(X, theta)
    val gradient_0 = sum((hX - y) :* X(::,0)) / m
    val gradients = Array.tabulate[Double](theta.length-1)(i => {
    ((sum((hX - y) :* X(::, (i.toInt+1))) / m) + (lambda / m * theta(i+1)))
    })
    (cost, DenseVector[Double](gradient_0 +: gradients))
  }

  def fminunc(costFn: (Theta) => (Double, DenseVector[Double]), initialTheta: DenseVector[Double], maxIterations: Int = 400): DenseVector[Double] = {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(theta: DenseVector[Double]): (Double, DenseVector[Double]) = costFn(theta)
    }
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter = maxIterations, m = 3)
    lbfgs.minimize(f, initialTheta)
  }


}
