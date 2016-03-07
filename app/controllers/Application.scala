package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.data.validation._

case class GenericCSVForm(box: String)

class Application extends Controller {

	
	val validateAsGenericCsvConstraint: Constraint[String] = Constraint("Check valid CSV")({
		plainText => {
		  	var someColMissing: Boolean = false
		  	var colIsNotDouble: Boolean = false
		  	val txtAsArray = plainText.split("\r\n")
		  	val numberOfColumns = txtAsArray.head.split(",").length
		  	for(row <- txtAsArray){
		  		val rowArray = row.split(",")
		  		if(rowArray.length != numberOfColumns) someColMissing = true
		  		for(col <- rowArray) {
		  			if(!Utils.isAllDigits(col)) colIsNotDouble = true
		  		}
		  	}
		    if(!someColMissing && !colIsNotDouble)
		    	Valid
		    else {
		    	if(someColMissing){
		    		Invalid(Seq(ValidationError("One of the lines does not have same number of columns")))
		    	} else if(colIsNotDouble){
		    		Invalid(Seq(ValidationError("All data should be numeric")))
		    	} else Invalid(Seq(ValidationError("There is some error in the entered CSV")))
		    }
		}
	})

	val inForm: Form[GenericCSVForm] = Form(
		mapping(
			"box" -> nonEmptyText.verifying(validateAsGenericCsvConstraint)
		)(GenericCSVForm.apply)(GenericCSVForm.unapply)
	)

	def index = Action {
		Ok(views.html.index(inForm)("Butter Learn"))
	}	

	def getAllHypothesis = Action{implicit request => 
		inForm.bindFromRequest.fold(
			formWithErrors => {
			    // binding failure, you retrieve the form containing errors:
			    BadRequest(views.html.index(formWithErrors)("Butter Learn"))
			},
			str => {
			    /* binding success, you get the actual value. */
			    val enteredString = str.box
			    Ok(enteredString)
			}
		)
	}

}
