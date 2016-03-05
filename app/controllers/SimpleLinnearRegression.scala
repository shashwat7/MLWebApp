package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.data.validation._
import ml._

class SimpleLinnearRegression extends Controller {

	val validateAsCsvConstraint: Constraint[String] = Constraint("Check 2 value CSV")({
		plainText => {
		  	var incorrect: Boolean = false
		  	for(row <- plainText.split("\r\n")){
		  		if(row.split(",").length != 2) incorrect = true
		  	}
		    if(!incorrect)
		    	Valid
		    else {
		    	Invalid(Seq(ValidationError("CSV is not valid")))
		    }
		}
	})

	val inForm: Form[TextBoxModel] = Form(
		mapping(
			"box" -> nonEmptyText.verifying(validateAsCsvConstraint)
		)(TextBoxModel.apply)(TextBoxModel.unapply)
	)

  def index = Action {
    Ok(views.html.slrLandingPage(inForm))
  }

  def getHypothesis = Action{ implicit request =>
	inForm.bindFromRequest.fold(
	  formWithErrors => {
	    // binding failure, you retrieve the form containing errors:
	    BadRequest(views.html.slrLandingPage(formWithErrors))
	  },
	  str => {
	    /* binding success, you get the actual value. */
	    val enteredString = str.box
	    // Construct an array of training sets
	    val arr: Array[(Double,Double)] = enteredString.split("\r\n").map(row => {
	    	val trainingEx = row.split(",")
	    	(trainingEx(0).toDouble, trainingEx(1).toDouble)
	    })
	    val theta = ml.SimpleLinnearRegression.with2var(arr)

	    Ok(views.html.slrHypothesis(theta))
	  }
	)
  }

}

case class TextBoxModel(box: String)