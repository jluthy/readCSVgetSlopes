# Load required libraries
library(readr)
library(ggplot2)
library(hexbin)

# Read in csv file generated from FlowJo export. You will need to set the apprpriate working directory where
# you have your exported csv file saved. Here I imported the file that I saved as 'a1cells.csv' from FlowJo.
# 
setwd("/Users/10273835/Documents/FlowJo_Shared_Data/Jay/")
data <- read.csv("a3.csv", header=TRUE, sep = ",")

getPlot(data) 
getSlope(data)

# This will generate a plot with a trend line using ggplot2.
ggplot(data, mapping = aes(x = VioletSSC.A, y = VioletSSC.H)) +
  geom_point()

# This will take the saved data and plot it with trendline
getPlot <- function(data){
  myDotPlot <- ggplot(data, mapping = aes(x = VioletSSC.A, y = VioletSSC.H))
  
  myDotPlot + 
    geom_hex() +
    geom_smooth(method = "lm")
}

# This will get the slope of the trendline and then print in as formula y=mx+b 
getSlope <- function(data){
  model <- lm(VioletSSC.A ~ VioletSSC.H, data=data)
  coef(model)
  paste('y =', coef(model)[[2]], '* x', '+', coef(model)[[1]])
}

#This saves the above ggplot to variable for easy testing with additional layers
myDotPlot <- ggplot(data, mapping = aes(x = VioletSSC.A, y = VioletSSC.H))

myDotPlot + 
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(VioletSSC.A ~ VioletSSC.H, data=data)
coef(model)
paste('y =', coef(model)[[2]], '* x', '+', coef(model)[[1]])


# Another plot option to bin the points into hexagonal bins
myDotPlot + 
  geom_hex() +
  geom_smooth(method = "lm")

                      