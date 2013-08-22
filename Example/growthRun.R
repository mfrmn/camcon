# install.packages('devtools')
library(devtools)
install_github(repo = "camcon", username = "mfrmn")
library(camcon)

rm(list=ls())

# Point to R file
rfile <- paste(getwd(),"/Example/growth.R",sep='/')
texqfile <- paste(getwd(),"/Example/tex/Quiz.tex",sep='/')

# Specify Number of Groups
nGrps <- 2

# Run script
camcon(rfile = rfile, texqfile = texqfile, ngrps = nGrps)
#camcon(rfile = rfile, texqfile = texqfile, ngrps = nGrps, strpattern = c('#DATA','#PARAMS','#SCRIPT','#END'), qpattern = '#Q', pngs = T, debug=F)