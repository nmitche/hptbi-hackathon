################################################################################
#
################################################################################
# FSS Model
#
# Args:
#   data a data.frame resulting from a call to prepare_fss_data()
#
# Return:
#   An R object.  This object will have the "hackathon_fss_model" class
#   prepended to it such that a call to predict can be used to generate
#   predictions from the training and testing data sets.
#
fss_model <- function(data) {

  ##############################################################################
  # User code starts here
  library(randomForest)

  #preparing subsets for model eval   
  sampleInd<-sample(1:nrow(data),.8*nrow(data))
  train_<-data[sampleInd,]
  holdout<-data[-sampleInd,]
  
  
  #fitting model
  rtn <-randomForest(fss~.,data=train,mtry=6)
  
  #holdout evaluation
  pred_rtn<-predict(rtn,newdata=subset(holdout,select=-fss))
  holdout_fss<-holdout_$fss
  holdout_mse<-mean((pred_rtn-holdout_fss)^2)  
  # User code ends here
  ##############################################################################

  class(rtn) <- c("hackathon_fss_model", class(rtn))
  rtn
}

################################################################################
# Predict Hackathon Mortality Model
#
# An S3 function call for hackathon_fss_model
#
# Args:
#   object  a hackathon_fss_model object
#   newdata a data.frame
#   ...     additional arguments passed through.  Not expected to be used as
#           part of the hackathon.
#
# Return:
#   A numeric vector of length equal to the nrow(newdata) representing the
#   predicted total FSS score.
#

predict.hackathon_fss_model <- function(object, newdata, ...) {

  ##############################################################################
  # user defined code starts here

  as.integer(stats::predict.lm(object, newdata, type = "response", ...))

}

################################################################################
#                                 End of File
################################################################################
