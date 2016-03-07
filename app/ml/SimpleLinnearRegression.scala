package ml

import breeze.linalg._
import breeze.numerics._
import play.api.Logger
import breeze.stats.mean
import breeze.stats.variance
/*
** This class is the implementation of linnear regression
*/
object SimpleLinnearRegression{

	def withNVar(X: DenseMatrix[Double], y: DenseVector[Double], m: Int, n: Int, alpha: Double, accuracy: Double): DenseVector[Double] = {
		X := featureNormalization(X)._1
		val theta = DenseVector.zeros[Double](n+1)
		gradientDescent(addXZero(X), y, theta, alpha, accuracy)
	}

	def featureNormalization(X: DenseMatrix[Double]): (DenseMatrix[Double],Transpose[DenseVector[Double]], Transpose[DenseVector[Double]]) = {
		val n: Int = X.cols // number of features
		val mu = mean(X(::,*))
		val sigma = variance(X(::,*))
		for(i <- 0 to (n-1)){
			X(::,i) :+= -1 * mu(i)
			X(::,i) :*= 1 / sigma(i)
		}
		(X, mu, sigma)
	}

	def addXZero(X: DenseMatrix[Double]): DenseMatrix[Double] = {
		val m = X.rows
		val n = X.cols
		val temp = DenseMatrix.zeros[Double](m,n+1)
		temp(::, 0) := DenseVector.ones[Double](m)
		temp(::, 1 to n) := X(:: , 0 to n-1)
		temp
	}

	def computeCost(X: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double]): Double = {
		sum(pow(((X * theta) - y),2)) / (2 * X.rows)
	}

	def gradientDescent(X: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double], alpha: Double, accuracy: Double): DenseVector[Double] = {
		var isAccurate = false
		val updatedTheta = theta
		var cost = 999999.9
		do {
			val delta = (X.t * ((X * theta) - y))
			delta :/= X.rows.toDouble
			updatedTheta :-= delta * alpha
			cost = computeCost(X,y,updatedTheta)
			Logger.debug("Cost: " + cost)
		} while(cost > accuracy)
		updatedTheta
	}

	def with2var(trainingData: Array[(Double,Double)]) : (Double,Double) = {
		// This function returns value of (theta0, theta1)
		Logger.debug("Following is the training set recieved")
		trainingData.foreach(e => Logger.debug(e.toString))
		val m = trainingData.length
		Logger.debug("m = " + m)
		val alpha = 0.1
		val accuracy = 0.0001
		var theta0, theta1 = 1.0
		var correctionIntheta0,correctionIntheta1 = 99999999.9
		val hyphothesisAtX = (x: Double) => (theta0 + theta1 * x)
		while(Math.abs(correctionIntheta0) > accuracy || Math.abs(correctionIntheta1) > accuracy){
			val derivativeOfTheta0 = sumOverTrainingSet(trainingData, (x: Double,y: Double) => (hyphothesisAtX(x) - y)) / m
			val derivativeOfTheta1 = sumOverTrainingSet(trainingData, (x: Double, y: Double) => ((hyphothesisAtX(x)-y)*x)) / m
			correctionIntheta0 = alpha * derivativeOfTheta0
			correctionIntheta1 = alpha * derivativeOfTheta1
			Logger.debug("Correction in theta: "+correctionIntheta0 + "," + correctionIntheta1)
			theta0 = theta0 - correctionIntheta0 
			theta1 = theta1 - correctionIntheta1
			Logger.debug("Updated theta: " + theta0 + "," + theta1)
		}
		(theta0, theta1)
	}

	def sumOverTrainingSet(set: Array[(Double, Double)], fn:((Double,Double)=>Double)): Double = {
		var sum: Double = 0.0
		for((x,y) <- set){
			sum = sum + fn(x, y)
		}
		// Logger.debug("sumOverTrainingSet returns: " + sum)
		sum
	}

}