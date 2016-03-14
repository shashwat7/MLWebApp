package controllers

import breeze.linalg.{DenseVector, DenseMatrix}
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.data.validation._
import ml._

class LogisticRegression extends Controller {

	val app = new Application
	val form = app.inForm

  def index = Action {
    Ok(views.html.logisticRegrLandingPage(form))
  }

  def getHypothesis = Action{ implicit request =>
    form.bindFromRequest.fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.logisticRegrLandingPage(formWithErrors))
      },
      str => {
        /* binding success, you get the actual value. */
        val enteredString = str.box
        val enteredStringAsArray = enteredString.split("\r\n")
        val m = enteredStringAsArray.length // Number of training examples
        val n = enteredStringAsArray.head.split(",").length - 1 // Number of features
        var X = new DenseMatrix(n,m,enteredStringAsArray.flatMap(row => row.split(",").dropRight(1).map(_.toDouble)),0).t // Input
        val y = DenseVector(enteredStringAsArray.map(row => row.split(",").last.toDouble)) // Output
        val alpha = 0.1
        val accuracy = 0.001
        val theta = ml.SimpleLinnearRegression.withNVar(X,y,m,n,alpha,accuracy)
        Ok("theta = " + theta.toString())
      }
    )
  }

}