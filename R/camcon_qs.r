#' Collects starting position of each new question
#'
#' Outputs the location (line number) of every match of qpattern (default #Q) in the
#' appropriate script
#'
#' @param qscript line by line list produced from question section of the original R file
#' @param qpattern pattern to identify location of each unique question
getqlocs <- function(qscript, qpattern) {
  qlocs <- grep(qpattern, qscript)
  if(length(qlocs) > 0) {
    for (i in 1:length(qlocs)) {
      qline <- qscript[qlocs[i]]
      if(length(grep("\\[",qline)) > 0) {
        names(qlocs)[i] <- qtovar(strsplit(qline, "\\[")[[1]][1])
      }
      else names(qlocs)[i] <- qtovar(qline)
    }
    qlocs
  }
  else stop(paste('Could not find any questions. Was looking for pattern:\n    ', qpattern))
}

#' Attempts to convert a question name into a valid variable name
#'
#' Removes # and " " and replaces "-" with "_" before converting question name to
#' lower case, and then checks whether is a valid variable name
#'
#' @param qname string to convert into a valid question name
qtovar <- function(qname) {
  qname <- gsub("#", "", qname)
  qname <- gsub("-", "_", qname)
  qname <- gsub(" ", "", qname)
  qname <- tolower(qname)
  validqs <- valid_var_names(qname)
  if (FALSE %in% validqs) stop(paste('Invalid or duplicated question name(s):\n    ', paste(qname[!validqs], collapse=', ')))
  qname
}

#' Determines whether string is a valid variable name
#'
#' Custom function to determine whether the new string generated from the question string is
#' a valid variable name
#'
#' @param qname string to convert into a valid question name
valid_var_names <- function(qname) {
  ok <- rep.int(TRUE, length(qname))
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(qname) > max_name_length] <- FALSE
  ok[qname != make.names(qname, unique = TRUE)] <- FALSE
  ok
}

#' Writes question section to the camcon script
#'
#' Creates the loop that goes through and creates unique solutions based on the individual
#' data sets and parameters 
#'
#' @param qscript line by line list produced from question section of the original R file
#' @param qlocs locations in qscript of starting points of each question and question names
#' @param ngrps number of unique solution sets to produce
#' @param cc_rfile location of output file to be written to during replication process
createqs <- function(qscript, qlocs, ngrps, cc_rfile) {
  text <- paste(qscript, collapse='\n  ')
  qnames <- names(qlocs)
  qexist <- paste('c("',paste(qnames, collapse='","'),'")',sep='')
  qvec <- gsub('\\"','',qexist)
  cat('\n\n# =================================================#',
      '\n# QUESTIONS: All questions to loop over',
      '\n# =================================================#',
      '\ncamcon_sols <- list()\n',
      'for(i in 1:',ngrps,') {\n',
      '  ', text,
      '\n\n  camcon_validq <- ', qexist, '\n',
      '  if(i == 1) {\n',
      '    if(FALSE %in% sapply(camcon_validq, function(x) exists(x))) stop("Inconsistent variable name(s). Expected but did not find:\n    ",paste(paste(camcon_validq[!sapply(camcon_validq, function(x) exists(x))], collapse=', ')))\n',
      '  }\n\n',
      '  camcon_sols[[i]] <- ', qvec, '\n',
      '  rm(list=camcon_validq)\n',
      '}', sep='', file = cc_rfile, append=T)
  getqparams(qscript, qlocs)
}

#' Outputs points assigned to question, question type and associated parameter
#'
#' Looks to ensure that required parameters are specified for each question and converts
#' these parameters into a list
#'
#' @param qscript line by line list produced from question section of the original R file
#' @param qlocs locations in qscript of starting points of each question and question names
getqparams <- function(qscript, qlocs) {
  params <- c()
  for(i in 1:length(qlocs)) {
    qline <- qscript[qlocs[i]]
    npts <- ""
    qtype <- ""
    qparam <- ""
    if(length(grep("\\[",qline)) > 0) {
      qstr <- strsplit(qline, "\\[")[[1]][2]
      if(length(grep("\\]",qstr)) > 0) {
        qstr <- gsub("\\]","",qstr)
        qstr <- gsub(" ","",qstr)
        if(nchar(qstr) > 0) {
          firstnum <- str_locate(qstr,"[0-9\\.]+")
          firstword <- str_locate(qstr,"[[:alpha:]]+")
          tmpstr <- ""
          secondnum <- str_locate(qstr,"js afk ask")
          # if a number exists at all then a second number may also..
          if(!is.na(firstnum[,2]) & firstnum[,2] < nchar(qstr)) {
            tmpstr <- substr(qstr, firstnum[,2]+1, nchar(qstr))
            secondnum <- str_locate(tmpstr,"[0-9\\.]+")
          }
          # if a word exists at all then this must be the question type
          if(!is.na(firstword[,1])) qtype <- str_extract(qstr,"[[:alpha:]]+")
          # if both a number and word exist
          if(!is.na(firstnum[,2]) & !is.na(firstword[,1])) {
            if(firstnum[,2] < firstword[,1]) {
              npts <- str_extract(qstr,"[0-9\\.]+")
              if(!is.na(secondnum[,2])) qparam <- str_extract(tmpstr,"[0-9\\.]+")
            }
            else {
              qparam <- str_extract(qstr,"[0-9\\.]+")
            }
          }
          # if a number but no word exists
          if(!is.na(firstnum[,2]) & is.na(firstword[,1])) {
            if(!is.na(secondnum[,2])) {
              npts <- str_extract(qstr,"[0-9\\.]+")
              qparam <- str_extract(tmpstr,"[0-9\\.]+")
            }
            else qparam <- str_extract(qstr,"[0-9\\.]+")
          }
          
          if(npts != "") {
            npts <- suppressWarnings(as.numeric(npts))
            if(is.na(npts)) stop(paste('Invalid first question parameter (# points), must be either blank or numeric, in line:\n    ', qline, sep=""))
          }
          if(qtype != "") if(!(qtype %in% c("NUM","SC","MC",'TXT'))) stop(paste('Invalid second question parameter (question type), must be either blank or NUM, MC, or TXT, in line:\n    ', qline, sep=""))
          if(qtype %in% c("SC","MC")) {
            if(qparam != "") {
              if(is.na(suppressWarnings(as.integer(qparam)))) stop(paste('Invalid `number of choices` options, must be an integer, in line:\n    ', qline, sep=""))
              if((as.integer(qparam) - as.numeric(qparam)) != 0) stop(paste('Invalid `number of choices` options, must be an integer, in line:\n    ', qline, sep=""))
              if(as.integer(qparam) <= 1) stop(paste('Invalid `number of choices` options, must be an integer greater than 1, in line:\n    ', qline, sep=""))
            }
            else stop(paste('Expected `number of choices` parameter for single/multiple choice question in line:\n    ', qline, sep=""))
          }
          if(qtype == "TXT") {
            if(qparam != "") {
              if(is.na(suppressWarnings(as.integer(qparam)))) stop(paste('Invalid `max answer length` options, must be an integer, in line:\n    ', qline, sep=""))
              if((as.integer(qparam) - as.numeric(qparam)) != 0) stop(paste('Invalid `max answer length` options, must be an integer, in line:\n    ', qline, sep=""))
              if(as.integer(qparam) < 1) stop(paste('Invalid `max answer length` options, must be an integer greater than 0, in line:\n    ', qline, sep=""))
            }
            else stop(paste('Expected `max answer length` parameter for text question in line:\n    ', qline, sep=""))
          }
          if(qtype == "") {
            if(qparam != "") {
              if(is.na(suppressWarnings(as.integer(qparam)))) stop(paste('Invalid parameter, must be an integer, in line:\n    ', qline, sep=""))
              if((as.integer(qparam) - as.numeric(qparam)) != 0) stop(paste('Invalid parameter, must be an integer, in line:\n    ', qline, sep=""))
            }
          }
        }
      }
      else stop(paste('Could not find end of question parameters specified by ] in line:\n    ', qstr))
    }
    params[[i]] <- list(points=npts, type=qtype, opts=qparam)
  }
  names(params) <- names(qlocs)
  params
}