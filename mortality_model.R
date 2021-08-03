################################################################################
#
################################################################################
# Mortality Model
#
# Args:
#   data a data.frame resulting from a call to prepare_mortality_data()
#
# Return:
#   An R object.  This object will have the "hackathon_mortality_model" class
#   prepended to it such that a call to predict can be used to generate
#   predictions from the training and testing data sets.
#
mortality_model <- function(data) {

  ##############################################################################
  # User code starts here
  library(randomForest)
  #dividing into training and test set
  #data<-prepare_mortality_data()
  sampleInd<-sample(1:nrow(data),.98*nrow(data))
  train<-data[sampleInd,]
  holdout<-data[-sampleInd,]
   
  #fitting model
  rtn <-randomForest(mortality~.,data=train,mtry=6)
  
  #making predictions on holdout data
  pred_rtn<-predict(rtn,newdata=subset(holdout,select=-mortality))
   
  #evaluating model
  holdout_mort<-holdout$mortality
  accuracy_mort_holdout<-mean(pred_rtn==holdout_mort)
  #the model had perfect accuracy(1) on *holdout, a small subset of the training data. This provides some evidence that the model will perform well on the testing set, and generalized well.
  # User code ends here
  ##############################################################################

  class(rtn) <- c("hackathon_mortality_model", class(rtn))
  rtn
}

################################################################################
# Predict Hackathon Mortality Model
#
# An S3 function call for hackathon_mortality_model
#
# Args:
#   object  a hackathon_mortality_model object
#   newdata a data.frame
#   ...     additional arguments passed through.  Not expected to be used as
#           part of the hackathon.
#
# Return:
#   A character vector of length equal to the nrow(newdata) with values
#   "Mortality" and "Alive"
#
predict.hackathon_mortality_model <- function(object, newdata, ...) {
  
  ##############################################################################
  # User Defined data preparation code starts here
  newdata_<-newdata 
  NextMethod(object,newdata_)
}

###manual predictions- code check test, uncomment single line hashtags to test
#d2<-prepare_mortality_data()
#f2<-mortality_model(d2)
#rf2 <-randomForest(mortality~.,data=d2[sample(1:nrow(d2),.98*nrow(d2)),],mtry=6)
#test_<-d2[-sample(1:nrow(d2),.98*nrow(d2)),]
#predict(rf2,newdata=test_,select=-mortality)
##above works!
##below line causes error: C stack usage 15924192 is too close too limit, is something wrong with the mortality model function?
#predict(f2,newdata=test,select=-mortality)
##because of above the custom prediction function will also have an error:
#predict.hackathon_mortality_model(m2,holdout)
################################################################################
#                                 End of File
################################################################################
