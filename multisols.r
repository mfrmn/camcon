#
# file  = name of file to be duplicated
# ngrps = number of groups
makesolutions <- function(rfile, ngrps, strpattern = c('#DST','#PST','#RST','#END'), qpattern = '#q') {
  con <- file(rfile)
  script <- readLines(con, warn=F)
  close(con)
  strlocs <- getstrlocs(script, strpattern)
  makeheader(script, rfile, strlocs$he)
  qlocs <- getqlocs(script, qpattern)
  data.files <- getdata(script, strlocs$ds, strlocs$de)
  createalldata(data.files, ngrps, rfile)
  params <- getparams(script, strlocs$ps, strlocs$pe)
  createallparams(params, ngrps, rfile)
}

getstrlocs <- function(script, strpattern) {
  head.end <- match(strpattern[1], script) - 1
  data.start <- head.end + 2
  data.end <- match(strpattern[2], script) - 1
  param.start <- data.end + 2
  param.end <- match(strpattern[3], script) - 1
  qs.start <- param.end + 2
  qs.end <- match(strpattern[4], script) - 1
  list(he=head.end,ds=data.start,de=data.end,ps=param.start,pe=param.end,qs=qs.start,qe=qs.end)
}

makeheader <- function(script, rfile, end) {
  for (i in 1:end) {
    write(script[i], file = paste("camcon_", rfile, sep=""), append=T)
  }
}

getqlocs <- function(script, qpattern) {
  qlocs <- grep(qpattern, script)
  qnames <- sub("#", "", script[qlocs])
  names(qlocs) <- qnames
  qlocs
}

getdata <- function(script, start, end) {
  data <- sapply(start:end, function(i) sub("#", "", script[i]))
  empties <- which(data == '')
  if(length(empties) > 0) data <- data[-empties]
  else data
}

createalldata <- function(alldata, ngrps, rfile) {
  root <- paste(getwd(), "camcon_data/", sep="/")
  if(!file.exists(root)) dir.create(root)
  for(i in 1: ngrps) {
    rootG <- paste(root, "group_", i, "/", sep="")
    if(!file.exists(rootG)) dir.create(rootG)
  }
  for(i in 1:length(alldata)) {
    createdata(alldata[i], ngrps, root, rfile)
  }
}

createdata <- function(data, ngrps, root, rfile) {
  cat('\n',data,'.ls <- list()\n',
      'n <- nrow(',data,')\n',
      'folder <- "',root,'"\n',
#       'folder <- paste("',root,'","',data,'",sep="")\n',
#       'if(!file.exists(folder)) dir.create(folder)\n',
      'for(i in 1:',ngrps,') {\n',
      '  toRem <- (i %% ',ngrps,') + seq(1, n, by=',ngrps,')\n',
      '  ',data,'.ls[[i]] <- ' ,data,'[-toRem,]\n',
      '  file_path <- paste(paste(paste(folder,"group",sep=""),i,sep="_"),"/',data,'.csv",sep="")\n',
#       '  file_path <- paste(paste(paste(folder,"/',data,'",sep=""),i,sep="_"),".csv",sep="")\n',
      '  write.csv(',data,'.ls[[i]], file_path, row.names = FALSE)\n',
      '}\n',
      'rm(n); rm(folder); rm(file_path); rm(i); rm(toRem)', sep='', file = paste("camcon_", rfile, sep=""), append=T)
}

getparams <- function(script, start, end) {
  params <- script[start:end]
  empties <- which(params == '')
  if(length(empties) > 0) params <- params[-empties]
  gsub(' ','',params)
}

createallparams <- function(allparams, ngrps, rfile) {
  for(i in 1:length(allparams)) {
    createparam(allparams[i], ngrps, rfile)
  }
}

createparam <- function(param, ngrps, rfile) {
  if(length(grep('<-', param)) > 0) {
    lhs <- unlist(strsplit(param,'<-'))[1]
    rhs <- unlist(strsplit(param,'<-'))[2]  
  }
  if(length(grep('=', param)) > 0) {
    lhs <- unlist(strsplit(param,'<-'))[1]
    rhs <- unlist(strsplit(param,'<-'))[2]  
  }
  if(exists('lhs')&exists('rhs')) {
    par <- eval(parse(text=rhs))
    if(length(par) == ngrps) cat('\n',param,sep='', file = paste("camcon_", rfile, sep=""), append=T)
    if(length(par) > ngrps) cat('\n',param,'[1:',ngrps,']',sep='', file = paste("camcon_", rfile, sep=""), append=T)
    if(length(par) < ngrps) cat('\n',lhs,'<- unlist(sapply(1:',ngrps,', function(i) ',rhs,'))[1:',ngrps,']',sep='', file = paste("camcon_", rfile, sep=""), append=T)
  }
}
createallparams(c('param.x <- rnorm(12)','param.x <- c(0,1,2,3)'),10,filename)
createparam('param.x <- c(0,1,2,3)',3,filename)



lhs <- unlist(strsplit('param.x <- rnorm(1)','<-'))[1]
rhs <- unlist(strsplit('param.x <- rnorm(1)','<-'))[2]
cat(lhs,'<- unlist(sapply(1:',10,', function(i) ',rhs,'))[1:',10,']',sep='')
param.x <- unlist(sapply(1:10, function(i)  c(1,2,3)))[1:10]

makesolutions(filename, nGrps, strpattern = c('#DATA','#PARAM','#SCRIPT','#END'), qpattern = '#q')





# Point to R file
filename <- "Quiz4.r"

# Specify Number of Groups
nGrps <- 10



# Read in the lines of the r file
script <- readLines(rfile, warn=F)
len <- length(script)

# Generate list of names of data sets
DStart <- match("#DATA", script) + 1
DEnd <- match("#START", script) - 1
data <- sapply(DStart:DEnd, function(i) sub("#", "", script[i]))

# Fill out header of new R file
for (i in 1:(DStart-2)) {
  write(script[i], file=paste("new",rfile,sep=""),append=T)
}

# Reduces file to those lines we are interested in
script <- script[(DEnd+2):len]
len <- length(script)
finloc <- match("#END", script)

# Generates list of question start (and hence end) locations
qlocs <- c(grep("#q",script),finloc)
nqs <- length(qlocs)-1

# Define Wrapping for each Q
wrapS <- "unlist( lapply(1:"
wrapS <- paste(wrapS,nGrps,sep="")
wrapS <- paste(wrapS,", function(x) {\n")
wrapE <- "\n}))"

# Main Function
for (i in 1:nqs) {
  # Generate unique question variable
  qname <- sub("#", "", script[qlocs[i]])
  qname <- paste(qname, " <- ", sep="")
  # Determine whether the question spans more than 1 line
  diff <- qlocs[i+1]-qlocs[i]-2
  # Write the first line of the output
  output <- script[qlocs[i]+1]
  # If Q spans more than one line, add all of the other lines to output to
  if (diff > 0) {
    for (j in 1:diff) {
      output <- paste(output,script[qlocs[i]+1+j], sep="; \n")
    }
  }
  # Check for each data set in the output, and if match then append with [[x]] 
  for (k in 1:length(data)) {
    output <- gsub(data[k],paste(data[k],"[[x]]", sep=""),output)   
  }
  # Add the wrapping to the output
  output <- paste(wrapS,output,sep="")
  output <- paste(qname,output,sep="")
  output <- paste(output,wrapE,sep="")
  # Write the output to the new file
  write(output, file=paste("new",rfile,sep=""),append=T)
}

# Prepare solution csv file

# Vector or Question Names
Qnames <- "\nQnames <- c("
for (i in 1:nqs) {
  qname <- sub("#", "", script[qlocs[i]])
  qname <- sub("_", "-", qname)
  qname <- paste("\"",qname,sep="")
  qname <- paste(qname,"\"",sep="")
  if (i < nqs) qname <- paste(qname,",",sep="")
  else qname <- paste(qname,")",sep="")
  Qnames <- paste(Qnames,qname,sep="")
}
write(Qnames, file=paste("new",rfile,sep=""),append=T)

# Vector of Question Types (single or multiple only)
Qtype <- "Qtype <- sapply(1:nqs, function(i) ifelse(is.numeric(eval(parse(text=sub(\"-\",\"_\",Qnames[i])))[1]),\"number\",\"multiple\"))"
write(Qtype, file=paste("new",rfile,sep=""),append=T)

tolower(c("A","B"))
# Vector or Options
Qopts <- "Qopts <- sapply(1:nqs, function(i) ifelse(Qtype[i] == \"multiple\", \"1\", \"\")) \n#NOTE : Qopts must be overwriten with correct number of options"
write(Qopts, file=paste("new",rfile,sep=""),append=T)

# Assign points to each question
base <- floor(100/nqs)
points <- rep(base,nqs)
nshort <- 100-sum(points)
while(nshort > 0) {
  qfill <- max(1,floor(nshort/nqs))
  print(qfill)
  nfill <- floor(nshort/qfill)
  wfill <- sample(1:nqs, nfill, replace=F)
  points <- points+sapply(1:nqs, function(i) ifelse(i %in% wfill, qfill, 0))
  nshort <- 100-sum(points)
}
points <- as.character(points)
Qpoints <- "Qpoints <- c("
for (i in 1:nqs) {
  qpoint <- points[i]
  qpoint <- paste("\"",qpoint,sep="")
  qpoint <- paste(qpoint,"\"",sep="")
  if (i < nqs) qpoint <- paste(qpoint,",",sep="")
  else qpoint <- paste(qpoint,")",sep="")
  Qpoints <- paste(Qpoints,qpoint,sep="")
}
write(Qpoints, file=paste("new",rfile,sep=""),append=T)

# Create solution matrix
sol.mat <- sub("\nQnames <- ","",Qnames)
sol.mat <- gsub("-","_",sol.mat)
sol.mat <- gsub("\"","",sol.mat)
sol.mat <- sub("c","cbind",sol.mat)
sol.mat <- paste("\nsol.mat <- ", sol.mat, sep="")
sol.mat <- paste(sol.mat, "\nfor(i in 1:", sep="")
sol.mat <- paste(sol.mat, nqs, sep="")
sol.mat <- paste(sol.mat, ") {
    if (Qtype[i] == \"multiple\") sol.mat[,i] <- tolower(sol.mat[,i])
}", sep="")

write(sol.mat, file=paste("new",rfile,sep=""),append=T)

for (i in 1:nqs) {
  qname <- sub("#", "", script[qlocs[i]])
  qname <- paste("\"",qname,sep="")
  qname <- paste(qname,"\"",sep="")
  if (i < nqs) qname <- paste(qname,",",sep="")
  else qname <- paste(qname,")",sep="")
  Qnames <- paste(Qnames,qname,sep="")
}
sol.mat <- cbind()

# Final output function
final <- paste("\nnGrps <- ",nGrps,sep="")
pt1 <- paste("\nnQs <- ",nqs,sep="") ; final <- paste(final,pt1,sep="")
final <- paste(final, "\nsol.dat <- as.data.frame(matrix(NA,nrow=(nQs*nGrps), ncol=8))
 for (i in 1:nQs) {
     for (j in 1:nGrps) {
         sol.dat[(i-1)*nGrps+j,] <- c(Qnames[i],Qtype[i],j,0,Qpoints[i],sol.mat[j,i],Qopts[i],paste((j-1)*nQs+i-1,\".png\", sep=\"\"))\n}\n}",sep="")

write(final, file=paste("new",rfile,sep=""),append=T)

lastbit <- "\nwrite.table(sol.dat, \"solutions.csv\", sep=\";\", row.names = FALSE, col.names=FALSE)"
write(lastbit, file=paste("new",rfile,sep=""),append=T)