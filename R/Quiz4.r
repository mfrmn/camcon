# Set directory
 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")
 setwd("~/Desktop/MPO1A/Contest 4")

# Load necessary packages
 library(TSA) ; library(dynlm) ; library(rugarch) ; library(forecast) ; library(FinTS)

# Declare custom functions
  arch.check <- function(model,maxlag=40) {
     chklags <- c(1:10,seq(15,min(maxlag, length(model$residuals)), by=5))
     output <- matrix(NA, nrow=length(chklags), ncol=3)
     colnames(output) <- c("lags","statistic","p-value")
     for (i in 1:length(chklags)) {
         archout <- ArchTest(c(model$res), lags=chklags[i])
         output[i,] <- c(chklags[i],archout$statistic,archout$p.value)
     }
     output
 }
 arch.check.2 <- function(resids,maxlag=40) {
     chklags <- c(1:10,seq(15,min(maxlag, length(resids)), by=5))
     output <- matrix(NA, nrow=length(chklags), ncol=3)
     colnames(output) <- c("lags","statistic","p-value")
     for (i in 1:length(chklags)) {
         archout <- ArchTest(c(resids), lags=chklags[i])
         output[i,] <- c(chklags[i],archout$statistic,archout$p.value)
     }
     output
 }

data(dji30ret)
n <- 21
stockret <- list()
str(dji30ret)
dji30.dat <- dji30ret[3237:nrow(dji30ret),]
dji30.dat <- dji30.dat[,-c(8,14,18,20)]

# Generate files
for(i in 1:n){
 folder = paste("~/Desktop/MPO1A/Contest 4/Data/stock_", i, sep="")
 folder = paste(folder, ".csv", sep="")
 data <- as.data.frame(dji30.dat[,i])
 colnames(data) <- c(colnames(dji30.dat)[i])
 write.csv(as.ts(data), folder, row.names = FALSE)
 stockret[[i]] = as.ts(data)
}
 
#DATA
#stockret
#START
#q1_i
adf.test.1(stockret, int=T, trend=T, k=8)$p.value
#q1_ii
lmA <- adf.test.2(stockret, int=T, trend=T, k=8)
ifelse(min(ljung.box.test.1(lmA$resid, seq(1,20,1))[,2]) > 0.05, "a", "b")
#q1_iii
auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)$arma[1]
#q1_iv
auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)$arma[2]
#q1_v
arma.mod <- auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)
ifelse(min(arch.check(arma.mod)[,3]) < 0.05, "a", "b")
#q2_i
arma.mod <- auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)
nar <- arma.mod$arma[1]
nma <- arma.mod$arma[2]
spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(7,0)), mean.model = list(armaOrder=c(nar,nma), include.mean=F))
arch.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec)
rob.mat <- arch.mod@fit$robust.matcoef
length(which((rob.mat[(nar+nma+2):nrow(rob.mat),4]) > 0.05))
#q2_ii
arma.mod <- auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)
nar <- arma.mod$arma[1]
nma <- arma.mod$arma[2]
spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(7,0)), mean.model = list(armaOrder=c(nar,nma), include.mean=F))
arch.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec)
nNA <- which(is.na(arch.mod@fit$resid)==F)
arch.sresid <- arch.mod@fit$residuals / arch.mod@fit$sigma
ifelse(min(arch.check.2(arch.sresid)[,3]) < 0.05, "a", "b")
#q2_iii
arma.mod <- auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)
nar <- arma.mod$arma[1]
nma <- arma.mod$arma[2]
spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(nar,nma), include.mean=F))
garch.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec)
nNA <- which(is.na(garch.mod@fit$resid)==F)
garch.sresid <- garch.mod@fit$residuals / garch.mod@fit$sigma
ifelse(min(ljung.box.test.1(garch.sresid[nNA], seq(0,50,1))) > 0.05, "a", "b")
#q2_iv
arma.mod <- auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)
nar <- arma.mod$arma[1]
nma <- arma.mod$arma[2]
spec <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(nar,nma), include.mean=F))
egarch.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec)
nNA <- which(is.na(egarch.mod@fit$resid)==F)
egarch.sresid <- egarch.mod@fit$residuals / egarch.mod@fit$sigma
ifelse(min(arch.check.2(egarch.sresid)[,3]) < 0.05, "a", "b")
#q2_v
arma.mod <- auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)
nar <- arma.mod$arma[1]
nma <- arma.mod$arma[2]
spec <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(nar,nma), include.mean=F, archm=T))
egarchm.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec)
rob.mat <- egarchm.mod@fit$robust.matcoef
ifelse(rob.mat[nar+nma+1,4] < 0.05, "a", "b")
#q2_vi
arma.mod <- auto.arima(stockret, stationary=T, max.P=0, max.Q=0, d=0, D=0)
nar <- arma.mod$arma[1]
nma <- arma.mod$arma[2]
spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(7,0)), mean.model = list(armaOrder=c(nar,nma), include.mean=F))
arch.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec, out.sample=50)
arch.forc <- ugarchforecast(arch.mod, n.ahead=50)
spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(nar,nma), include.mean=F))
garch.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec, out.sample=50)
garch.forc <- ugarchforecast(garch.mod, n.ahead=50)
spec <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(nar,nma), include.mean=F))
egarch.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec, out.sample=50)
egarch.forc <- ugarchforecast(egarch.mod, n.ahead=50)
spec <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(nar,nma), include.mean=F, archm=T))
egarchm.mod <- ugarchfit(data=as.data.frame(stockret), spec=spec, out.sample=50)
egarchm.forc <- ugarchforecast(egarchm.mod, n.ahead=50)
forecasts <- cbind(fpm(arch.forc),fpm(garch.forc),fpm(egarch.forc),fpm(egarchm.forc))
which.min(forecasts[1,])
#END