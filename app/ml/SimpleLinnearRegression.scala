package ml

import play.api.Logger
/*
** This class is the implementation of linnear regression
*/
object SimpleLinnearRegression{

	def withNVar(trainingData: Array[String]) = {
		val n: Int = trainingData.head.split(",").length

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