################################################################################
# Prepare FSS Data
#
# Define data procesing steps to apply to the data set used to train and test
# models for predicting total FSS.
#
# Args:
#   training  (logicial) if the data set to read in is the training or testing
#             data set.
#
# Return:
#   A data.frame with the defnined primary outcome and any user specific
#   elements needed for training and testing their model.
#
prepare_fss_data <- function(training = TRUE) {

  # import the data set
  if (!training & file.exists("./csvs/testing.csv")) {
    hackathon_fss_data <- read.csv(file = "./csvs/testing.csv")
  } else {
    hackathon_fss_data <- read.csv(file = "./csvs/training.csv")
  }

  # Define the primary outcome -- do not edit this.  If you need the outcome in
  # a different format, e.g., integer or logical, create an additional
  # data.frame element in user defined code section below.
  hackathon_fss_data$fss_total <-
    Reduce(function(x, y) {x + y},
           x = hackathon_fss_data[grep("^fss", names(hackathon_fss_data))])

  # subset to known FSS values
  hackathon_fss_data <-
    subset(hackathon_fss_data, hospdisposition != "Mortality")
  hackathon_fss_data <-
    subset(hackathon_fss_data, !is.na(fss_total))

  ##############################################################################
  # User Defined data preperation code starts here
  library(dplyr)

  hackathon_fss_data_<- hackathon_fss_data%>%
    select(-starts_with('admitto'))
  
  #percent missing data (by columns):
  100*(ncol(hackathon_fss_data)-ncol(hackathon_fss_data_))/ncol(hackathon_fss_data)
  
  
  # removing remainder of missing data
  hackathon_fss_data_<-na.omit(hackathon_fss_data_)
  hackathon_fss_data<-hackathon_fss_data_

  # User Defined Code ends here
  ##############################################################################

  hackathon_fss_data
}

################################################################################
#                                 End of File
################################################################################
