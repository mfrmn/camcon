#' Collects data section
#'
#' Outputs list of all non-empty lines specified in data section
#'
#' @param rscript line by line list produced from R file for replication
#' @param start an integer specifying the index value in script at which the data section starts
#' @param end an integer specifying the index value in script at which the data section ends
getdata <- function(rscript, start, end) {
  if(end > start) {
    data <- rscript[start:end]
    empties <- which(data == '')
    if(length(empties) > 0) data <- data[-empties]
    data
  }
  else NA
}

#' Loops through data creation
#'
#' Main data creation function that generates folders for data storage and then loops through
#' each line given by getdata() and generates a new data frame for each group. 
#'
#' @param alldata list generated by getdata() of all lines expected to contain data
#' @param ngrps number of unique solution sets to produce
#' @param cc_dir location of output directory to be written to during replication process
#' @param cc_rfile location of output file to be written to during replication process
createalldata <- function(alldata, ngrps, cc_dir, cc_rfile) {
  cat('\n\n# =================================================#',
      '\n# DATA: ALL DATA SETS TO RANDOMIZE',
      '\n# =================================================#', file = cc_rfile, append=T)
  if(sum(!is.na(alldata)) != 0) {
    for(i in 1:ngrps) {
      gr_folder <- paste(cc_dir, "data_", i, "/", sep="")
      if(!file.exists(gr_folder)) dir.create(gr_folder)
    }
    data.names <- rep(NA, length(alldata))
    for(i in 1:length(alldata)) {
      params <- detectdatatype(alldata[i])
      data.names[i] <- createdata(alldata[i], params, ngrps, cc_dir, cc_rfile)
    }
    if(sum(is.na(data.names)) < length(alldata)) data.names[!is.na(data.names)]
    else NA
  }
  else NA
}

#' Check whether cross-sectional, time series, or panel data
#'
#' Detects the type of data, and ensures that valid associated parameters are supplied
#'
#' @param data a single line generated by getdata() that contains the data set and specified parameters
detectdatatype <- function(data) {
  datatype <- ""
  dataparam <- ""
  if(length(grep("#", data)) > 0) {
    datacomment <- strsplit(data, '#')[[1]][2]
    datacomment <- gsub("\\s","",datacomment)
    datacomment <- gsub("\\[","",datacomment)
    datacomment <- gsub("\\]","",datacomment)
    if(length(grep("CS", datacomment, ignore.case=T)) > 0) {
      datatype <- "cs"
      if(length(gregexpr(",", datacomment)[[1]]) == 1)  {
        datacomment <- strsplit(datacomment, ',')[[1]][2]
        dataparam <- suppressWarnings(as.numeric(datacomment))
        if(is.na(dataparam)) stop(paste('Invalid `proportion to drop` parameter, must be numeric, in line\n    ', data, sep=""))
        if(dataparam >= 1 || dataparam <= 0) stop(paste('Invalid `proportion to drop` parameter, must be strictly between 0 and 1, in line\n    ', data, sep=""))
        dataparam <- list(propdrop = dataparam)
      }
      else stop(paste('Expected one parameter:\n    - the proportion of the data set you wish to drop\nin line\n    ', data, sep=""))
    }
    if(length(grep("TS", datacomment, ignore.case=T)) > 0) {
      datatype <- "ts"
      if(length(gregexpr(",", datacomment)[[1]]) == 1)  {
        datacomment <- strsplit(datacomment, ',')[[1]][2]
        dataparam <- suppressWarnings(as.numeric(datacomment))
        if(is.na(dataparam)) stop(paste('Invalid standard deviation parameter, must be numeric, in line\n    ', data, sep=""))
        if(dataparam <= 0) stop(paste('Invalid standard deviation parameter, must be greater than 0, in line\n    ', data, sep=""))
        dataparam <- list(normsd = dataparam)
      }
      else stop(paste('Expected one parameter:\n    - the standard deviation of the random noise\nin line\n    ', data, sep=""))
    }
    if(length(grep("PN", datacomment, ignore.case=T)) > 0) {
      datatype <- "pn"
      if(length(gregexpr(",", datacomment)[[1]]) == 2)  {
        dataprop <- strsplit(datacomment, ',')[[1]][2]
        dataprop <- suppressWarnings(as.numeric(dataprop))
        if(is.na(dataprop)) stop(paste('Invalid `proportion to drop`, must be numeric, parameter in line\n    ', data, sep=""))
        if(dataprop >= 1 || dataprop <= 0) stop(paste('Invalid `proportion to drop` parameter, must be strictly between 0 and 1, in line\n    ', data, sep=""))
        dataindex <- strsplit(datacomment, ',')[[1]][3]
        if(length(grep("[^[:graph:]]", dataindex)) > 0) stop(paste('Invalid id parameter in line\n    ', data, sep=""))
        dataparam <- list(propdrop = dataprop, index = dataindex)
      }
      else stop(paste('Expected two parameters:\n    - the proportion of the data set you wish to drop\n- the unique individual identifier\nin line\n    ', data, sep=""))
    }
  }
  else stop(paste('Failed to specify parameters of data set in line:\n    ', data, sep=""))
  if(datatype == "") stop(paste('Invalid data type specified (must be either CS, TS, or PN) in line\n    ', data, sep=""))
  list(type = datatype, param = dataparam)
}

#' Finds the name and source of the data file
#'
#' Uses a line given by getdata() and detects the data frame name and the source of the data,
#' and generates a new data frame for each group depending on the data type.
#'
#' @param data a single line generated by getdata() that contains the data set and specified parameters
#' @param params the parameters identified by detectdatatype()
#' @param ngrps number of unique solution sets to produce
#' @param cc_dir location of output directory to be written to during replication process
#' @param cc_rfile location of output file to be written to during replication process
createdata <- function(data, params, ngrps, cc_dir, cc_rfile) {
  data <- strsplit(data, '#')[[1]][1]
  data.len <- nchar(data)
  if(length(grep('<-', data)) > 0) {
    pos <- regexpr('<-',data)[1]
    lhs <- substr(data, 1, pos-1)
    rhs <- substr(data, pos+2, data.len)
  }
  if(!exists('lhs') & !exists('rhs') & (length(grep('=', data)) > 0)) {
    pos <- regexpr('=',data)[1]
    lhs <- substr(data, 1, pos-1)
    rhs <- substr(data, pos+1, data.len)
  }
  if(exists('lhs')) lhs <- gsub(" ", "", lhs)
  if(exists('lhs') & exists('rhs')) {
    if(params$type == 'cs') createcsdata(data, params$param$propdrop, lhs, rhs, ngrps, cc_dir, cc_rfile)
    if(params$type == 'ts') createtsdata(data, params$param$normsd, lhs, rhs, ngrps, cc_dir, cc_rfile)
    if(params$type == 'pn') createcsdata(data, params$param$propdrop, params$param$index, lhs, rhs, ngrps, cc_dir, cc_rfile)
  }
  if(exists('lhs') & !exists('rhs')) cat('\n', lhs, '\n', sep='', file = cc_rfile, append=T)
                                         
  if(exists('lhs') & exists('rhs')) lhs
  else NA
}

#' Writes to camcon script for cross-sectional data
#'
#' Generates a new data frame for every individual by dropping random rows from the data frame
#'
#' @param data a single line generated by getdata() that contains the data set and specified parameters
#' @param propdrop the proportion of rows to drop
#' @param lhs the name of the original data frame
#' @param rhs the input for the original data frame
#' @param ngrps number of unique solution sets to produce
#' @param cc_dir location of output directory to be written to during replication process
#' @param cc_rfile location of output file to be written to during replication process
createcsdata <- function(data, propdrop, lhs, rhs, ngrps, cc_dir, cc_rfile) {
  toRem <- paste('toRem <- sample(1:n,max(1,round(n*',propdrop,')), replace=F)',sep='')
  cat('\n',lhs,'<-',rhs,'\n',
      'if(!exists("', lhs, '")) stop("Cannot find data frame: ', lhs, '")\n',
      'n <- nrow(',lhs,')\n',
      'if(n*',propdrop,'<1) warning("Parameter `proportion to drop` too small to introduce variation, so one row dropped at random instead for data frame: ', lhs, '", call. = FALSE)\n',
      lhs,'.ls <- list()\n',
      'folder <- "',cc_dir,'"\n',
      'for(i in 1:',ngrps,') {\n',
      '  ',toRem,'\n',
      '  ',lhs,'.ls[[i]] <- ' ,lhs,'[-toRem,]\n',
      '  file_path <- paste(paste(paste(folder,"data",sep=""),i,sep="_"),"/',strsplit(lhs,"[^[:alnum:]]")[[1]][1],'.csv",sep="")\n',
      '  write.csv(',lhs,'.ls[[i]], file_path, row.names = FALSE)\n',
      '}\n',
      'rm(n); rm(folder); rm(file_path); rm(i); rm(toRem)', sep='', file = cc_rfile, append=T)
}


#' Writes to camcon script for time-series data
#'
#' Generates a new data frame for every individual by adding random noise
#'
#' @param data a single line generated by getdata() that contains the data set and specified parameters
#' @param sd the standard deviation for the random noise
#' @param lhs the name of the original data frame
#' @param rhs the input for the original data frame
#' @param ngrps number of unique solution sets to produce
#' @param cc_dir location of output directory to be written to during replication process
#' @param cc_rfile location of output file to be written to during replication process
createtsdata <- function(data, sd, lhs, rhs, ngrps, cc_dir, cc_rfile) {
  cat('\n',lhs,'<-',rhs,'\n',
      'if(!exists("', lhs, '")) stop("Cannot find data frame: ', lhs, '")\n',
      'n <- length(',lhs,')\n',
      lhs,'.ls <- list()\n',
      'folder <- "',cc_dir,'"\n',
      'for(i in 1:',ngrps,') {\n',
      '  ',lhs,'.ls[[i]] <- ' ,lhs,' + rnorm(n, 0, ', sd,') \n',
      '  file_path <- paste(paste(paste(folder,"data",sep=""),i,sep="_"),"/',strsplit(lhs,"[^[:alnum:]]")[[1]][1],'.csv",sep="")\n',
      '  write.csv(',lhs,'.ls[[i]], file_path, row.names = FALSE)\n',
      '}\n',
      'rm(n); rm(folder); rm(file_path); rm(i)', sep='', file = cc_rfile, append=T)
}

#' Writes to camcon script for panel data
#'
#' Generates a new data frame for every individual by dropping random individuals
#'
#' @param data a single line generated by getdata() that contains the data set and specified parameters
#' @param propdrop the proportion of rows to drop
#' @param index the unique index for individuals
#' @param lhs the name of the original data frame
#' @param rhs the input for the original data frame
#' @param ngrps number of unique solution sets to produce
#' @param cc_dir location of output directory to be written to during replication process
#' @param cc_rfile location of output file to be written to during replication process
createpndata <- function(data, propdrop, index, lhs, rhs, ngrps, cc_dir, cc_rfile) {
  stop("Option to use panel data not currently supported")
}
