#' Generates file for upload to camcon.eu
#'
#' This function takes a suitable .R input file and (optionally) .tex question sheet
#' and produces from this unique datasets, calculates individual answers, and
#' (optionally) creates .png solutions and individual question sheets for direct
#' upload to camcon.eu.
#'
#' @param rfile the R file for replication, must have specific structure (see documentation)
#' @param ngrps number of unique solution sets to produce
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
#' @param qpattern pattern to identify location of each unique question
#' @param pngs if T then produces .png solutions unique to each group for upload to camcon.eu
#' @param debug if T then post-processing does not occur, i.e. does not delete variables or intermediary files used by camcon()
#' @export
camcon <- function(rfile, ngrps, strpattern = c('#DATA','#PARAMS','#SCRIPT','#END'), qpattern = '#Q', propdrop=0.1, pngs = T, debug = F) {
  origFiles <- ls(envir=globalenv())
  con <- file(rfile); script <- readLines(con, warn=F); close(con)
  valid_params(ngrps, strpattern, qpattern, pngs, debug, script)
  cc_str <- camcon_init(rfile)
  gen_out <- gen_script(ngrps, strpattern, qpattern, script, cc_str, propdrop)
  source(cc_str$file)
  gen_csv(ngrps, cc_str$dir, gen_out$qlocs)  
  if(pngs) pnganswers(gen_out$orig_qscript, gen_out$qlocs, ngrps, cc_str$dir)
  if(!debug) tidyup(cc_str$file, origFiles)
  message(paste('Camcon output placed in folder:',cc_str$dir,'\n--------------'))
}

#' Check for valid group number
#'
#' Throws error if group number is not a positive integer
#'
#' @param ngrps number of unique solution sets to produce
valid_grp_num <- function(ngrps) {
  if(!is.numeric(ngrps)) stop('Invalid number of groups (ngrps). Must be numeric.')
  if((as.integer(ngrps) - ngrps) != 0) stop('Invalid number of groups (ngrps). Must be an integer.')
  if(ngrps < 1) stop('Invalid number of groups (ngrps). Must be a positive integer.')
}

#' Check for valid structure identifiers
#'
#' Throws error if structure identifier (i) is not specified, (ii) does not start
#' with a hash, or (iii) none / multiple occurrences are identified in script.
#'
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
valid_strpattern <- function(strpattern, script) {
  if(length(strpattern) != 4) stop('Invalid length for parameter strpattern. Expected four entries.')
  hash_start <- (regexpr('#', strpattern) == 1)
  if(FALSE %in% hash_start) stop(paste('Each element in parameter strpattern must begin with a hash (#). Check:\n    ', paste(strpattern[!hash_start], collapse=', ')))
  uniq_strpattern <- lapply(sapply(strpattern, function(i) grep(i, script)), length) == 1
  if(FALSE %in% uniq_strpattern) stop(paste('Found none or multiple occurrences of element(s) of strpattern in script. Each must only appear once. Check:\n    ', paste(strpattern[!uniq_strpattern], collapse=', ')))
}

#' Check for valid question identifier
#'
#' Throws error if question identifier (i) is not specified, (ii) does not start
#' with a hash, or (iii) no occurrences are identified in script.
#'
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
valid_qpattern <- function(qpattern, script) {
  if(length(qpattern) != 1) stop('Invalid length for parameter qpattern. Expected one entry')
  if(regexpr('#', qpattern) != 1) stop('Parameter qpattern must begin with a hash (#)')
  if(length(grep(qpattern, script)) == 0) stop('Found no occurrences of qpattern in script')
}

#' Check for valid initialising parameters in camcon() function
#'
#' Throws error if invalid parameter given for ngrps, strpattern, qpattern
#' pngs or debug.
#'
#' @param ngrps number of unique solution sets to produce
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
#' @param qpattern pattern to identify location of each unique question
#' @param pngs if T then produces .png solutions unique to each group for upload to camcon.eu
#' @param debug if T then post-processing does not occur, i.e. does not delete variables or intermediary files used by camcon()
#' @param script line by line list produced from R file for replication
valid_params <- function(ngrps, strpattern, qpattern, pngs, debug, script) {
  valid_grp_num(ngrps)
  valid_strpattern(strpattern, script)
  valid_qpattern(qpattern, script)
  if(!is.logical(pngs)) stop('Parameter "pngs" should be logical (i.e. TRUE or FALSE)')
  if(!is.logical(debug)) stop('Parameter "debug" should be logical (i.e. TRUE or FALSE)')
}

#' Creates folder for camcon files and blank script
#'
#' Creates folder of format /camcon_filename/ for storing data files,
#' .csv file with solutions, and .png files, and blank script for camcon
#' replication.
#'
#' @param rfile name of the R file being replicated through camcon
camcon_init <- function(rfile) {
  fexten <- gsub(' ', '', rfile)
  if(length(grep('/', fexten)) > 0) fexten <- unlist(strsplit(fexten,'/'))[length(unlist(strsplit(fexten,'/')))]
  if(length(grep('\\.', fexten)) > 0) fexten <- unlist(strsplit(fexten,'\\.'))[1]
  
  camcon_root <- paste(getwd(), "/camcon_", fexten, '/', sep="")
  if(file.exists(camcon_root)) unlink(camcon_root, recursive=T) # delete folder if already exists
  dir.create(camcon_root)
  
  camcon_file <- paste("camcon_", fexten, '.R', sep="")
  if(file.exists(camcon_file)) file.remove(camcon_file) # delete file if already exists
  file.create(camcon_file)
  
  list(dir=camcon_root, file=camcon_file)
}

#' Matches location of sections in script
#'
#' Creates a vector of matched string locations.
#'
#' @param script line by line list produced from R file for replication
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
getstrlocs <- function(script, strpattern) {
  head.end <- match(strpattern[1], script) - 1; if(is.na(head.end)) stop(paste('Cannot find "data" header, specified by pattern:\n    ', strpattern[1]))
  data.end <- match(strpattern[2], script) - 1; if(is.na(data.end)) stop(paste('Cannot find "parameter" header, specified by pattern:\n    ', strpattern[2]))
  param.end <- match(strpattern[3], script) - 1; if(is.na(param.end)) stop(paste('Cannot find "script" header, specified by pattern:\n    ', strpattern[3]))
  qs.end <- match(strpattern[4], script) - 1; if(is.na(qs.end)) stop(paste('Cannot find "end" location, specified by pattern:\n    ', strpattern[4]))
  data.start <- head.end + 2; param.start <- data.end + 2; qs.start <- param.end + 2
  list(he=head.end,ds=data.start,de=data.end,ps=param.start,pe=param.end,qs=qs.start,qe=qs.end)
}

#' Outputs header information to file
#'
#' Writes all lines from the start of the script up until the specified end location
#' to a new file.
#'
#' @param script line by line list produced from R file for replication
#' @param ccfile name of output file to be written to during replication process
#' @param end an integer specifying the index value in script at which the header ends
makeheader <- function(script, ccfile, end) {
  cat('# =================================================#',
      '\n# HEADER: BLANK FOR NOW, CAN PUT ANYTHING HERE',
      '\n# =================================================#\n', file = ccfile, append=T)
  for (i in 1:end) {
    write(script[i], file = ccfile, append=T)
  }
}

#' Collects data frames
#'
#' Outputs list of all data frames specified in data section
#'
#' @param script line by line list produced from R file for replication
#' @param start an integer specifying the index value in script at which the data section starts
#' @param end an integer specifying the index value in script at which the data section ends
getdata <- function(script, start, end) {
  if(end > start) {
    data <- script[start:end]
    empties <- which(data == '')
    if(length(empties) > 0) data <- data[-empties]
    data
  }
  else NA
}

createalldata <- function(alldata, ngrps, ccroot, ccfile, propdrop) {
  cat('\n\n# =================================================#',
      '\n# DATA: BLANK FOR NOW, CAN PUT ANYTHING HERE',
      '\n# =================================================#', file = ccfile, append=T)
  if(sum(is.na(alldata)) == 0) {
    for(i in 1: ngrps) {
      gr_folder <- paste(ccroot, "group_", i, "/", sep="")
      if(!file.exists(gr_folder)) dir.create(gr_folder)
    }
    data.names <- rep(NA, length(alldata))
    for(i in 1:length(alldata)) {
      data.names[i] <- createdata(alldata[i], ngrps, ccroot, ccfile, propdrop)
    }
    if(sum(is.na(data.names)) < length(alldata)) data.names[!is.na(data.names)]
    else NA
  }
  else NA
}

createdata <- function(data, ngrps, ccroot, ccfile, propdrop) {
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
    toRem <- paste('toRem <- sample(1:n,max(1,round(n*',propdrop,')), replace=F)',sep='')
    cat('\n',lhs,'<-',rhs,'\n',
        'if(!exists("', lhs, '")) stop("Cannot find data frame: ', lhs, '")\n',
        'n <- nrow(',lhs,')\n',
        'if(round(n*',propdrop,')<1) warning("Variable propdrop too small to introduce variation, so one row dropped at random instead for data frame: ', lhs, '", call. = FALSE)\n',
        lhs,'.ls <- list()\n',
        'folder <- "',ccroot,'"\n',
        'for(i in 1:',ngrps,') {\n',
        '  ',toRem,'\n',
#         '  toRem <- (i %% ',ngrps,') + seq(1, n, by=',ngrps,')\n',
        '  ',lhs,'.ls[[i]] <- ' ,lhs,'[-toRem,]\n',
        '  file_path <- paste(paste(paste(folder,"group",sep=""),i,sep="_"),"/',lhs,'.csv",sep="")\n',
        '  write.csv(',lhs,'.ls[[i]], file_path, row.names = FALSE)\n',
        '}\n',
        'rm(n); rm(folder); rm(file_path); rm(i); rm(toRem)', sep='', file = ccfile, append=T)
  }
  if(exists('lhs')) lhs
  else NA
}

getparams <- function(script, start, end) {
  if(end > start) {
    params <- script[start:end]
    params <- gsub(" ", "", params)
    empties <- which(params == '')
    if(length(empties) > 0) params <- params[-empties]
    params
  }
  else NA
}

createallparams <- function(allparams, ngrps, ccfile) {
  cat('\n\n# =================================================#',
      '\n# PARAMS: BLANK FOR NOW, CAN PUT ANYTHING HERE',
      '\n# =================================================#', file = ccfile, append=T)
  if(sum(is.na(allparams)) == 0) {
    param.names <- rep(NA, length(allparams))
    for(i in 1:length(allparams)) {
      param.names[i] <- createparam(allparams[i], ngrps, ccfile)
    }
    if(sum(is.na(param.names)) < length(allparams)) param.names[!is.na(param.names)]
    else NA
  }
  else NA
}

createparam <- function(param, ngrps, ccfile) {
  param.len <- nchar(param)
  if(length(grep('<-', param)) > 0) {
    pos <- regexpr('<-',param)[1]
    lhs <- substr(param, 1, pos-1)
    rhs <- substr(param, pos+2, param.len)
  }
  if(!exists('lhs') & !exists('rhs') & (length(grep('=', param)) > 0)) {
    pos <- regexpr('=',param)[1]
    lhs <- substr(param, 1, pos-1)
    rhs <- substr(param, pos+1, param.len)
  }
  if(exists('lhs') & exists('rhs')) {
    par <- eval(parse(text=rhs))
    if(length(par) == ngrps) cat('\n',param, sep='', file = ccfile, append=T)
    if(length(par) > ngrps) cat('\n',param,'[1:',ngrps,']', sep='', file = ccfile, append=T)
    if(length(par) < ngrps) cat('\n',lhs,' <- unlist(sapply(1:',ngrps,', function(i) ',rhs,'))[1:',ngrps,']',sep='', file = ccfile, append=T)
  }
  if(exists('lhs')) lhs
  else NA
}

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
      qscript <- gsub(params[i], paste(params[i],"[[i]]", sep=""), qscript)
    }
  }
  list(orig=orig,new=qscript)
}

valid_var_names <- function(x) {
  ok <- rep.int(TRUE, length(x))
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(x) > max_name_length] <- FALSE
  ok[x != make.names(x, unique = TRUE)] <- FALSE
  ok
}

qtovar <- function(qnames) {
  qnames <- gsub("#", "", qnames)
  qnames <- gsub("-", "_", qnames)
  qnames <- gsub(" ", "", qnames)
  qnames <- tolower(qnames)
  validqs <- valid_var_names(qnames)
  if (FALSE %in% validqs) stop(paste('Invalid or duplicated question name(s):\n    ', paste(qnames[!validqs], collapse=', ')))
  qnames
}

getqlocs <- function(script, qpattern) {
  qlocs <- grep(qpattern, script)
  if(length(qlocs) > 0) {
    names(qlocs) <-  qtovar(script[qlocs])
    qlocs
  }
  else stop(paste('Could not find any questions. Was looking for pattern:\n    ', qpattern))
}

createqs <- function(qscript, qlocs, ngrps, ccfile) {
  text <- paste(qscript, collapse='\n  ')
  qnames <- names(qlocs)
  qexist <- paste('c("',paste(qnames, collapse='","'),'")',sep='')
  qvec <- gsub('\\"','',qexist)
  cat('\n\n# =================================================#',
      '\n# QUESTIONS: BLANK FOR NOW, CAN PUT ANYTHING HERE',
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
      '}', sep='', file = ccfile, append=T)
}

gen_script <- function(ngrps, strpattern, qpattern, script, cc_str, propdrop) {
  strlocs <- getstrlocs(script, strpattern)
  makeheader(script, cc_str$file, strlocs$he)
  data <- createalldata(getdata(script, strlocs$ds, strlocs$de), ngrps, cc_str$dir, cc_str$file, propdrop)
  params <- createallparams(getparams(script, strlocs$ps, strlocs$pe), ngrps, cc_str$file)
  if((sum(is.na(data)) != 0) & (sum(is.na(params)) != 0)) stop('Did not find any data or parameter vectors.')
  qscript <- dpreplace(script[strlocs$qs:strlocs$qe], data, params)
  orig_qscript <- qscript$orig; qscript <- qscript$new
  qlocs <- getqlocs(qscript, qpattern)
  createqs(qscript, qlocs, ngrps, cc_str$file)
  list(qlocs=qlocs, orig_qscript=orig_qscript)
}

all.is.numeric <- function (x) {
  old <- options(warn = -1)
  on.exit(options(old))
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  !any(is.na(as.numeric(x)))
}

getqtype <- function(ngrps) {
  if(!exists('camcon_sols')) stop('Cannot find solutions')
  if(length(unique(unlist(lapply(camcon_sols, length)))) != 1) stop('Missing one or more answers in solution matrix camcon_sols')
  if(length(camcon_sols) != ngrps) stop('Missing solutions for one or more groups')
  sol.matrix <- do.call(rbind, camcon_sols)
  qtype <- rep(NA, ncol(sol.matrix))
  for(i in 1:ncol(sol.matrix)) {
    if(all.is.numeric(sol.matrix[,i])) qtype[i] <- 'number'
    else {
      if(unique(nchar(sol.matrix[,i])) == 1) qtype[i] <- 'multiple'
      else qtype[i] <- 'text'
    }
  }
  qtype
}

getqoptions <- function(qlocs, qtype) {
  qnamesopts <- paste(names(qlocs),'_options',sep='')
  qoptions <- rep('', length(qlocs))
  mchoice <- which(qtype == 'multiple')
  if(length(mchoice) > 0) {
    mcopts <- sapply(qnamesopts[mchoice], exists)
    if(FALSE %in% mcopts) stop(paste('Please specify number of options for multiple choice question(s):\n    ', paste(names(qlocs[!mcopts]), collapse=', ')))
    for(i in mchoice) {
      qoptions[i] <- eval(parse(text=qnamesopts[i]))
    }
  }
  mcopts_numeric <- sapply(sub('','0',qoptions), all.is.numeric)
  if(FALSE %in% mcopts_numeric) stop(paste('Expected numeric value(s) for:\n    ', paste(qnamesopts[!mcopts_numeric], collapse=', ')))
  qoptions
}

getqpoints <- function(qlocs, totpoints) {
  nqs <- length(qlocs)
  base <- floor(totpoints/nqs)
  points <- rep(base,nqs)
  nshort <- totpoints-sum(points)
  while(nshort > 0) {
    qfill <- max(1,floor(nshort/nqs))
    nfill <- floor(nshort/qfill)
    wfill <- sample(1:nqs, nfill, replace=F)
    points <- points+sapply(1:nqs, function(i) ifelse(i %in% wfill, qfill, 0))
    nshort <- totpoints-sum(points)
  }
  points
}

answercsv <- function(qloc, qtype, qoptions, qpoints, ngrps, ccroot) {
  qnames <- gsub('q','',names(qloc))
  qnames <- gsub('_','-',qnames)
  sol.mat <- c()
  for(i in 1:ngrps) {
    for(j in 1:length(qnames)) {
      sol.mat <- rbind(sol.mat, cbind(qnames[j], qtype[j], i, 0, qpoints[j], camcon_sols[[i]][j], qoptions[j], paste('sols_G', i, '_Q', j, '.png', sep='')))
    }
  }
  write.table(sol.mat, file = paste(ccroot, 'sols_upload.csv', sep=''), append=T, col.names=F, row.names=F, sep=',')
}

gen_csv <- function(ngrps, dir, qlocs) {
  qtype <- getqtype(ngrps)
  qpoints <- getqpoints(qlocs, 100)
  qoptions <- getqoptions(qlocs, qtype)
  answercsv(qlocs, qtype, qoptions, qpoints, ngrps, dir)
}

pnganswers <- function(qscript, qlocs, ngrps, ccroot) {
  require(PerformanceAnalytics)
  root <- paste(ccroot, 'pngs/', sep="")
  if(!file.exists(root)) dir.create(root)
  locs <- c(qlocs, length(qscript)+1)
  for(i in 1:ngrps) {
    for(j in 1:(length(locs)-1)) {
      basecharperline <- 75; basenlines <- 25; cexval <- 1.5
      charperline <- floor(basecharperline / cexval)
      nline <- floor(basenlines / cexval)
      text <- c(qscript[locs[j]:(locs[j+1]-1)],'',paste('#ANS:',camcon_sols[[i]][j]))
      text.vec <- c()
      pos <- 1
      for(k in 1:length(text)) {
        text.vec[pos] <- text[k]
        while(nchar(text.vec[pos]) > charperline) {
          text.vec[pos+1] <- paste('·  ', substr(text.vec[pos],charperline+1,nchar(text.vec[pos])),sep='')
          text.vec[pos] <- substr(text.vec[pos],1,charperline)
          pos <- pos + 1
        }
        pos <- pos + 1
      }
      while(length(text.vec) > nline) {
        cexval <- cexval - 0.2
        charperline <- floor(basecharperline / cexval)
        nline <- floor(basenlines / cexval)
        text.vec <- c()
        pos <- 1
        for(k in 1:length(text)) {
          text.vec[pos] <- text[k]
          while(nchar(text.vec[pos]) > charperline) {
            text.vec[pos+1] <- paste('·  ', substr(text.vec[pos],charperline+1,nchar(text.vec[pos])),sep='')
            text.vec[pos] <- substr(text.vec[pos],1,charperline)
            pos <- pos + 1
          }
          pos <- pos + 1
        }
        if(cexval < 0.3) stop('Cannot fit solution on plot.')
      }
      png(filename = paste(root, 'sols_G', i,'_Q', j, '.png', sep=''), width=612, height=431)
      textplot(text.vec, valign='top', halign='left', cex=cexval, mar=c(0, 0.5, 0.5, 0))
      dev.off()
    }
  }
}

tidyup <- function(ccfile, origFiles) {
  file.remove(ccfile) 
  rm(list=ls(envir=globalenv())[!(ls(envir=globalenv()) %in% origFiles)], envir = globalenv())
}

#' @name growth
#' @title Needs a title
#' @description This data set needs a description.
#' @docType data
#' @usage growth
#' @format a 6x3 data frame
#' @source Find source
#' @author Find author
#' @export
NULL

