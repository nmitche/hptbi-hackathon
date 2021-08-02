################################################################################
# Prepare Mortality Data
#
# Define data processing steps to apply to the data set used to train and test
# models for predicting mortality.
#
# Args:
#   training  (logical) if the data set to read in is the training or testing
#             data set.
#
# Return:
#   A data.frame with the defined primary outcome and any user specific
#   elements needed for training and testing their model.
#
prepare_mortality_data <- function(training = TRUE) {

  # import the data set
  if (!training & file.exists("./csvs/testing.csv")) {
    hackathon_mortality_data <- read.csv(file = "./csvs/testing.csv")
  } else {
    hackathon_mortality_data <- read.csv(file = "./csvs/training.csv")
  }

  # Define the primary outcome -- do not edit this.  If you need the outcome in
  # a different format, e.g., integer or logical, create an additional
  # data.frame element in user defined code section below.
  hackathon_mortality_data$mortality <-
    as.integer(hackathon_mortality_data$hospdisposition == "Mortality")

  # Omit some elements - FSS is omitted from this data set.  FSS could not be
  # assessed for patients who died.  To reduce confusion FSS related elements
  # are omitted as missing values for FSS are be highly correlated with
  # mortality.
  hackathon_mortality_data <-
    hackathon_mortality_data[-grep("fss", names(hackathon_mortality_data))]

  ##############################################################################
  # User Defined Code starts here
  library(DataExplorer)
  library('dplyr')
  
  
  hackathon_mortality_data$gcs_use <-
    ifelse(is.na(hackathon_mortality_data$gcsed),
           yes = hackathon_mortality_data$gcsicu,
           no  = hackathon_mortality_data$gcsed)
  
  #The DataExplorer package was used to generate an automated EDA report
  #create_report(ds)

  
  #Based on report results missing data was quickly identified
  
  hackathon_mortality_data_<- hackathon_mortality_data%>%
    select(-starts_with('admitto'))
  
  #percent missing data (by columns):
  100*(ncol(hackathon_mortality_data)-ncol(hackathon_mortality_data_))/ncol(hackathon_mortality_data)
  

  
  hackathon_mortality_data_[['mortality']]<-as.factor(hackathon_mortality_data_[['mortality']])
  hackathon_mortality_data<-hackathon_mortality_data_
  # User Defined Code ends here
  ##############################################################################

  hackathon_mortality_data
}

################################################################################
#                                 End of File
################################################################################
