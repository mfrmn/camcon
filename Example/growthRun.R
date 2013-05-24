# install.packages('devtools')
# install.packages('roxygen2')
library(devtools)
devtools::document()
install_github(repo = "camcon", username = "mfrmn")
library(camcon)

rm(list=ls())

# Point to R file
filename <- paste(getwd(),"/Example/growth.R",sep='/')

# Specify Number of Groups
nGrps <- 2

# Run script
camcon(filename, nGrps, strpattern = c('#DATA','#PARAMS','#SCRIPT','#END'), qpattern = '#Q', pngs = T, propdrop=0.1, debug=F)