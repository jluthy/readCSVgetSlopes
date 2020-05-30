# Load required libraries
library(readr)

#' 1. Change the file path @setwd() to location for exporting .csv file from this analysis. 
#' 2. Change the file path @csv.import to location where your .csv files from FlowJo are located. 
#'    *Note* if there is a file that is missing the parameter names specified in the @getSlope function,
#'    you will get an error. Remove those files from folder before running this code.
#' 3. Run this code by either Highlighting all text and clikcing Run button.
#'    or run one line at a time. 
#'    *Note* After running this code, rename the exported "Results.csv" file.
#'
setwd("/Users/10273835/Documents/FlowJo_Shared_Data/Jay/rtest")


# This function will read in multiple csv files from the location specified when creating 
# the "csv.import" object.
import.multiple.csv.files<-function(mypath,mypattern,...)
{
  tmp.list.1 <- list.files(mypath, pattern = mypattern)
  tmp.list.2 <- list(length = length(tmp.list.1))
  for (i in 1:length(tmp.list.1)) {
    tmp.list.2[[i]] <- read.csv(tmp.list.1[i], ...)
  }
  names(tmp.list.2) <- tmp.list.1
  tmp.list.2
}

# Use above function like this to create an object "csv.import" 
# Specify the path in quotes below where you have exported your .csv files from FlowJo.
csv.import <- import.multiple.csv.files("/Users/10273835/Documents/FlowJo_Shared_Data/Jay/rtest", ".csv$", sep=",")

# Here is function to calculate the slope of trendline that is fit between VioletSSC-A & H parameters.
# It gets loaded into environment and used to create "result" object.
getSlope <- function(data){
  model <- lm(VioletSSC.A ~ VioletSSC.H, data=data)
  coef(model)
  paste('y =', coef(model)[[2]], '* x', '+', coef(model)[[1]])
}

# Generates the resulting list of results for all files.
result <- lapply(csv.import, getSlope)

# The will export the results as a new .csv file. This is optional step, otherwise view the "result" object in RStudio.
#' *Note* This will get exported to where you set the working directory above using "setwd()"
#' The .csv speadsheet will paste reults into multiple columns, so copy > paste transpose in excel if desired.
write.csv(result, "Results.csv")