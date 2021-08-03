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
  sampleInd<-sample(1:nrow(data),.98*nrow(data))
  train<-data[sampleInd,]
  holdout<-data[-sampleInd,]
  
  #fitting model
  rtn <-randomForest(fss_total~.,data=train,mtry=6)
  
  #holdout evaluation
  pred_rtn<-predict(rtn,newdata=subset(holdout,select=-fss_total))
  holdout_fss<-holdout$fss_total
  holdout_mse<-mean((pred_rtn-holdout_fss)^2) #.301 when ran 8-3-21  
  mean_fse<-mean(data$fss_total)  #9.265
  #model performed exceptionally well on the *holdout (a small subset of the training data), mse:.301. Note the mean of fss_total is 9.265. This provides some evidence that the model will perform well on the test set and generalized well.
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
  newdata_<-newdata
  NextMethod(object,newdata_)
}

###manual predictions-code check test, uncomment single hash tag lines to test
#d1<-prepare_fss_data()
#f1<-fss_model(d1)
#rf1 <-randomForest(fss_total~.,data=d1[sample(1:nrow(d1),.98*nrow(d1)),],mtry=6)
#test__<-d1[-sample(1:nrow(d1),.98*nrow(d1)),]
#predict(rf1,newdata=test__,select=-fss_total)
##above works!
##below line causes error: C stack usage 15923632 is too close too limit, is something wrong with the fss model function?
#predict(f1,newdata=test__,select=-fss_total)
##because of above the custom prediction function will also have an error:
#predict.hackathon_mortality_model(f1,test__)
################################################################################
#                                 End of File
################################################################################
