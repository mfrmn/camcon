#' Generates script used by camcon function to produce unique datasets and solutions.
#'
#' This function takes a suitable .R input file and (optionally) .tex question sheet
#' and produces from this unique datasets, calculates individual answers, and
#' (optionally) creates .png solutions and individual question sheets for direct
#' upload to camcon.eu.
#'
#' @param ngrps number of unique solution sets to produce
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
#' @param qpattern pattern to identify location of each unique question
#' @param rscript line by line list produced from R file for replication
#' @param struc location of output folder and file to be written to during replication process

gen_script <- function(ngrps, strpattern, qpattern, rscript, struc) {
  strlocs <- getstrlocs(rscript, strpattern)
  makeheader(rscript, struc$file, strlocs$he)
  data <- createalldata(getdata(rscript, strlocs$ds, strlocs$de), ngrps, struc$dir, struc$file)
  params <- createallparams(getparams(rscript, strlocs$ps, strlocs$pe), ngrps, struc$file)
  if((sum(!is.na(data)) == 0) & (sum(!is.na(params)) == 0)) stop('Did not find any data or parameter vectors.')
  qscript <- dpreplace(rscript[strlocs$qs:strlocs$qe], data, params)
  qlocs <- getqlocs(qscript$new, qpattern)
  qparams <- createqs(qscript$new, qlocs, ngrps, struc$file)
  list(qlocs=qlocs, orig_qscript=qscript$orig, qparams=qparams)
}

#' Matches location of sections in script
#'
#' Creates a vector of matched string locations.
#'
#' @param rscript line by line list produced from R file for replication
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
getstrlocs <- function(rscript, strpattern) {
  head.end <- match(strpattern[1], rscript) - 1; if(is.na(head.end)) stop(paste('Cannot find "data" header, specified by pattern:\n    ', strpattern[1]))
  data.end <- match(strpattern[2], rscript) - 1; if(is.na(data.end)) stop(paste('Cannot find "parameter" header, specified by pattern:\n    ', strpattern[2]))
  param.end <- match(strpattern[3], rscript) - 1; if(is.na(param.end)) stop(paste('Cannot find "script" header, specified by pattern:\n    ', strpattern[3]))
  qs.end <- match(strpattern[4], rscript) - 1; if(is.na(qs.end)) stop(paste('Cannot find "end" location, specified by pattern:\n    ', strpattern[4]))
  data.start <- head.end + 2; param.start <- data.end + 2; qs.start <- param.end + 2
  list(he=head.end,ds=data.start,de=data.end,ps=param.start,pe=param.end,qs=qs.start,qe=qs.end)
}

#' Replaces original data and parameters in script with new versions unique to each individual
#'
#' Goes through the question part of the script and every time it finds a match with one of the
#' names of a data frame or a parameter vector specified in the #DATA and #PARAMS sections
#' it replaces it with the a new version which was generated using createalldata() and createallparams()
#'
#' @param qscript line by line list produced from question section of the original R file
#' @param data the names of all of the detected data frames
#' @param params the names of all of the detected parameters
dpreplace <- function(qscript, data, params) {
  qscript <- qscript[qscript != '']
  orig <- qscript
  if(sum(is.na(data)) == 0) {
    for(i in 1:length(data)) {
      qscript <- gsub(data[i], paste(data[i],".ls[[i]]", sep=""), qscript)
    }
  }
  if(sum(is.na(params)) == 0) {
    for(i in 1:length(params)) {
      qscript <- gsub(params[i], paste(params[i],"[i]", sep=""), qscript)
    }
  }
  list(orig=orig,new=qscript)
}
