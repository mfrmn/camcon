# Specify Number of Groups
nGrps <- 21

# Point to R file
rfile <- "test.r"

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