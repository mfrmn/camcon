# install.packages('devtools')
# install.packages('roxygen2')
library(devtools)
devtools::document()
install_github(repo = "camcon", username = "mfrmn")
library(camcon)

rm(list=ls())

# Point to R file
filename <- paste(getwd(),"mba.R",sep='/')

# Specify Number of Groups
nGrps <- 10

# Run script
camcon(rfile = filename, nrgps = nGrps, strpattern = c('#DATA','#PARAMS','#SCRIPT','#END'), qpattern = '#Q', pngs = F, propdrop=0.01)