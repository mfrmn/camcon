## Simulate data
set.seed(123)
# two normal distributions of weight data
nd = round(rnorm(40,150,5),0); hist(nd,4)
d = round(nd + rnorm(40,-1,3),0); hist(d,4)
diet = data.frame(start=nd, followUp=d)
str(diet)

n = 7   # number of groups

g2 = list()   # list for variants of diet.csv

# Generate files
for(i in 1:n){
 folder = paste("~/Desktop/diet", i, sep="")
 folder = paste(folder, ".xls", sep="")
 write.csv(diet[-seq(i,nrow(diet),20),], folder, row.names = FALSE)
 g2[[i]] = diet[-seq(i,nrow(diet),20),]
}

q1_i <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "a","b"); 
mean(g2[[x]])
}))
q1_ii <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "b","a")
}))

Qnames <- c("q1-i","q1-ii")
Qtype <- sapply(1:nqs, function(i) ifelse(is.numeric(eval(parse(text=sub("-","_",Qnames[i])))[1]),"number","multiple"))
Qopts <- sapply(1:nqs, function(i) ifelse(Qtype[i] == "multiple", "1", "")) 
#NOTE : Qopts must be overwriten with correct number of options
Qpoints <- c("50","50")

sol.mat <- cbind(q1_i,q1_ii)
for(i in 1:2) {
    if (Qtype[i] == "multiple") sol.mat[,i] <- tolower(sol.mat[,i])
}

nGrps <- 21
nQs <- 2
sol.dat <- as.data.frame(matrix(NA,nrow=(nQs*nGrps), ncol=8))
 for (i in 1:nQs) {
     for (j in 1:nGrps) {
         sol.dat[(i-1)*nGrps+j,] <- c(Qnames[i],Qtype[i],j,0,Qpoints[i],sol.mat[j,i],Qopts[i],paste((j-1)*nQs+i-1,".png", sep=""))
}
}
## Simulate data
set.seed(123)
# two normal distributions of weight data
nd = round(rnorm(40,150,5),0); hist(nd,4)
d = round(nd + rnorm(40,-1,3),0); hist(d,4)
diet = data.frame(start=nd, followUp=d)
str(diet)

n = 7   # number of groups

g2 = list()   # list for variants of diet.csv

# Generate files
for(i in 1:n){
 folder = paste("~/Desktop/diet", i, sep="")
 folder = paste(folder, ".xls", sep="")
 write.csv(diet[-seq(i,nrow(diet),20),], folder, row.names = FALSE)
 g2[[i]] = diet[-seq(i,nrow(diet),20),]
}

q1_i <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "a","b"); 
mean(g2[[x]])
}))
q1_ii <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "b","a")
}))

Qnames <- c("q1-i","q1-ii")
Qtype <- sapply(1:nqs, function(i) ifelse(is.numeric(eval(parse(text=sub("-","_",Qnames[i])))[1]),"number","multiple"))
Qopts <- sapply(1:nqs, function(i) ifelse(Qtype[i] == "multiple", "1", "")) 
#NOTE : Qopts must be overwriten with correct number of options
Qpoints <- c("50","50")

sol.mat <- cbind(q1_i,q1_ii)
for(i in 1:2) {
    if (Qtype[i] == "multiple") sol.mat[,i] <- tolower(sol.mat[,i])
}

nGrps <- 21
nQs <- 2
sol.dat <- as.data.frame(matrix(NA,nrow=(nQs*nGrps), ncol=8))
 for (i in 1:nQs) {
     for (j in 1:nGrps) {
         sol.dat[(i-1)*nGrps+j,] <- c(Qnames[i],Qtype[i],j,0,Qpoints[i],sol.mat[j,i],Qopts[i],paste((j-1)*nQs+i-1,".png", sep=""))
}
}
## Simulate data
set.seed(123)
# two normal distributions of weight data
nd = round(rnorm(40,150,5),0); hist(nd,4)
d = round(nd + rnorm(40,-1,3),0); hist(d,4)
diet = data.frame(start=nd, followUp=d)
str(diet)

n = 7   # number of groups

g2 = list()   # list for variants of diet.csv

# Generate files
for(i in 1:n){
 folder = paste("~/Desktop/diet", i, sep="")
 folder = paste(folder, ".xls", sep="")
 write.csv(diet[-seq(i,nrow(diet),20),], folder, row.names = FALSE)
 g2[[i]] = diet[-seq(i,nrow(diet),20),]
}

q1_i <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "a","b"); 
mean(g2[[x]])
}))
q1_ii <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "b","a")
}))

Qnames <- c("q1-i","q1-ii")
Qtype <- sapply(1:nqs, function(i) ifelse(is.numeric(eval(parse(text=sub("-","_",Qnames[i])))[1]),"number","multiple"))
Qopts <- sapply(1:nqs, function(i) ifelse(Qtype[i] == "multiple", "1", "")) 
#NOTE : Qopts must be overwriten with correct number of options
Qpoints <- c("50","50")

sol.mat <- cbind(q1_i,q1_ii)
for(i in 1:2) {
    if (Qtype[i] == "multiple") sol.mat[,i] <- tolower(sol.mat[,i])
}

nGrps <- 21
nQs <- 2
sol.dat <- as.data.frame(matrix(NA,nrow=(nQs*nGrps), ncol=8))
 for (i in 1:nQs) {
     for (j in 1:nGrps) {
         sol.dat[(i-1)*nGrps+j,] <- c(Qnames[i],Qtype[i],j,0,Qpoints[i],sol.mat[j,i],Qopts[i],paste((j-1)*nQs+i-1,".png", sep=""))
}
}
## Simulate data
set.seed(123)
# two normal distributions of weight data
nd = round(rnorm(40,150,5),0); hist(nd,4)
d = round(nd + rnorm(40,-1,3),0); hist(d,4)
diet = data.frame(start=nd, followUp=d)
str(diet)

n = 7   # number of groups

g2 = list()   # list for variants of diet.csv

# Generate files
for(i in 1:n){
 folder = paste("~/Desktop/diet", i, sep="")
 folder = paste(folder, ".xls", sep="")
 write.csv(diet[-seq(i,nrow(diet),20),], folder, row.names = FALSE)
 g2[[i]] = diet[-seq(i,nrow(diet),20),]
}

q1_i <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "a","b"); 
mean(g2[[x]])
}))
q1_ii <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "b","a")
}))

Qnames <- c("q1-i","q1-ii")
Qtype <- sapply(1:nqs, function(i) ifelse(is.numeric(eval(parse(text=sub("-","_",Qnames[i])))[1]),"number","multiple"))
Qopts <- sapply(1:nqs, function(i) ifelse(Qtype[i] == "multiple", "1", "")) 
#NOTE : Qopts must be overwriten with correct number of options
Qpoints <- c("50","50")

sol.mat <- cbind(q1_i,q1_ii)
for(i in 1:2) {
    if (Qtype[i] == "multiple") sol.mat[,i] <- tolower(sol.mat[,i])
}

nGrps <- 21
nQs <- 2
sol.dat <- as.data.frame(matrix(NA,nrow=(nQs*nGrps), ncol=8))
 for (i in 1:nQs) {
     for (j in 1:nGrps) {
         sol.dat[(i-1)*nGrps+j,] <- c(Qnames[i],Qtype[i],j,0,Qpoints[i],sol.mat[j,i],Qopts[i],paste((j-1)*nQs+i-1,".png", sep=""))
}
}
## Simulate data
set.seed(123)
# two normal distributions of weight data
nd = round(rnorm(40,150,5),0); hist(nd,4)
d = round(nd + rnorm(40,-1,3),0); hist(d,4)
diet = data.frame(start=nd, followUp=d)
str(diet)

n = 7   # number of groups

g2 = list()   # list for variants of diet.csv

# Generate files
for(i in 1:n){
 folder = paste("~/Desktop/diet", i, sep="")
 folder = paste(folder, ".xls", sep="")
 write.csv(diet[-seq(i,nrow(diet),20),], folder, row.names = FALSE)
 g2[[i]] = diet[-seq(i,nrow(diet),20),]
}

q1_i <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "a","b"); 
mean(g2[[x]])
}))
q1_ii <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "b","a")
}))

Qnames <- c("q1-i","q1-ii")
Qtype <- sapply(1:nqs, function(i) ifelse(is.numeric(eval(parse(text=sub("-","_",Qnames[i])))[1]),"number","multiple"))
Qopts <- sapply(1:nqs, function(i) ifelse(Qtype[i] == "multiple", "1", "")) 
#NOTE : Qopts must be overwriten with correct number of options
Qpoints <- c("50","50")

sol.mat <- cbind(q1_i,q1_ii)
for(i in 1:2) {
    if (Qtype[i] == "multiple") sol.mat[,i] <- tolower(sol.mat[,i])
}

nGrps <- 21
nQs <- 2
sol.dat <- as.data.frame(matrix(NA,nrow=(nQs*nGrps), ncol=8))
 for (i in 1:nQs) {
     for (j in 1:nGrps) {
         sol.dat[(i-1)*nGrps+j,] <- c(Qnames[i],Qtype[i],j,0,Qpoints[i],sol.mat[j,i],Qopts[i],paste((j-1)*nQs+i-1,".png", sep=""))
}
}
write.table(sol.dat, "solutions.csv", sep=";", quote = "'", row.names = FALSE, col.names=FALSE)
write.table(sol.dat, "solutions.csv", sep=";", quote = "'", row.names = FALSE, col.names=FALSE)
write.table(sol.dat, "solutions.csv", sep=";", quote = "\'", row.names = FALSE, col.names=FALSE)
## Simulate data
set.seed(123)
# two normal distributions of weight data
nd = round(rnorm(40,150,5),0); hist(nd,4)
d = round(nd + rnorm(40,-1,3),0); hist(d,4)
diet = data.frame(start=nd, followUp=d)
str(diet)

n = 7   # number of groups

g2 = list()   # list for variants of diet.csv

# Generate files
for(i in 1:n){
 folder = paste("~/Desktop/diet", i, sep="")
 folder = paste(folder, ".xls", sep="")
 write.csv(diet[-seq(i,nrow(diet),20),], folder, row.names = FALSE)
 g2[[i]] = diet[-seq(i,nrow(diet),20),]
}

q1_i <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "a","b"); 
mean(g2[[x]])
}))
q1_ii <- unlist( lapply(1:21 , function(x) {
ifelse(t.test((g2[[x]]$followUp - g2[[x]]$start), alternative="less")$p.value < 0.05, "b","a")
}))

Qnames <- c("q1-i","q1-ii")
Qtype <- sapply(1:nqs, function(i) ifelse(is.numeric(eval(parse(text=sub("-","_",Qnames[i])))[1]),"number","multiple"))
Qopts <- sapply(1:nqs, function(i) ifelse(Qtype[i] == "multiple", "1", "")) 
#NOTE : Qopts must be overwriten with correct number of options
Qpoints <- c("50","50")

sol.mat <- cbind(q1_i,q1_ii)
for(i in 1:2) {
    if (Qtype[i] == "multiple") sol.mat[,i] <- tolower(sol.mat[,i])
}

nGrps <- 21
nQs <- 2
sol.dat <- as.data.frame(matrix(NA,nrow=(nQs*nGrps), ncol=8))
 for (i in 1:nQs) {
     for (j in 1:nGrps) {
         sol.dat[(i-1)*nGrps+j,] <- c(Qnames[i],Qtype[i],j,0,Qpoints[i],sol.mat[j,i],Qopts[i],paste((j-1)*nQs+i-1,".png", sep=""))
}
}

write.table(sol.dat, "solutions.csv", sep=";", row.names = FALSE, col.names=FALSE)
