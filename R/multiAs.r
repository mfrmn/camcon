# Specify Number of Groups
nGrps <- 21
setwd("~/Desktop")

# Point to R file
rfile <- "Quiz4As-small.Rnw"

# Read in the lines of the r file
script <- readLines(rfile, warn=F)
len <- length(script)

# Determine region to be repeated
DStart <- match("%STARTDUPLICATE", script) + 1
DEnd <- match("%ENDDUPLICATE", script) - 1

# Determine region to be altered
RStart <- match("%STARTREPLACE", script) + 2
REnd <- match("%ENDREPLACE", script) - 2

# Fill out header of new R file
for (i in 1:(DStart-2)) {
    write(script[i], file=paste("new",rfile,sep=""),append=T)
}

# Main Function
for (i in 1:nGrps) {
    for (j in DStart:DEnd) {
        if (j >= RStart && j <= REnd) {
            output <- paste("stockret <- stockrets[[", i, sep="")
            output <- paste(output, "]]", sep="")
        }
        else output <- script[j]
        write(output, file=paste("new",rfile,sep=""),append=T)
    }
}

write("\\end{document}", file=paste("new",rfile,sep=""),append=T)