#
# file  = name of file to be duplicated
# ngrps = number of groups
camcon <- function(rfile, ngrps, strpattern = c('#DATA','#PARAMS','#SCRIPT','#END'), qpattern = '#q') {
  con <- file(rfile)
  script <- readLines(con, warn=F)
  close(con)
  strlocs <- getstrlocs(script, strpattern)
  makeheader(script, rfile, strlocs$he)
  data.files <- getdata(script, strlocs$ds, strlocs$de)
  createalldata(data.files, ngrps, rfile)
  params <- createallparams(getparams(script, strlocs$ps, strlocs$pe), ngrps, rfile)
  qscript <- dpreplace(script[strlocs$qs:strlocs$qe], data.files, params)
  qlocs <- getqlocs(qscript, qpattern)
  createqs(qscript, qlocs, ngrps, rfile)
  source(paste("camcon_", rfile, sep=""))
  qtype <- getqtype(ngrps)
  qpoints <- getqpoints(qlocs, 100)
  qoptions <- getqoptions(qlocs, qtype)
  pnganswers(qscript, qlocs, ngrps, rfile)
  answercsv(qlocs, qtype, qoptions, qpoints, ngrps, rfile)
  tidyup(rfile)
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
  cat('# =================================================#', file = paste("camcon_", rfile, sep=""), append=T)
  cat('\n# HEADER: BLANK FOR NOW, CAN PUT ANYTHING HERE', file = paste("camcon_", rfile, sep=""), append=T)
  cat('\n# =================================================#\n', file = paste("camcon_", rfile, sep=""), append=T)
  for (i in 1:end) {
    write(script[i], file = paste("camcon_", rfile, sep=""), append=T)
  }
}

getdata <- function(script, start, end) {
  data <- sapply(start:end, function(i) sub("#", "", script[i]))
  empties <- which(data == '')
  if(length(empties) > 0) data <- data[-empties]
  else data
}

createalldata <- function(alldata, ngrps, rfile) {
  cat('\n\n# =================================================#', file = paste("camcon_", rfile, sep=""), append=T)
  cat('\n# DATA: BLANK FOR NOW, CAN PUT ANYTHING HERE', file = paste("camcon_", rfile, sep=""), append=T)
  cat('\n# =================================================#', file = paste("camcon_", rfile, sep=""), append=T)
  fexten <- sub(' ', '', rfile)
  if(length(grep('\\.', fexten)) > 0) fexten <- unlist(strsplit(fexten,'\\.'))[1]
  root <- paste(getwd(), "/camcon_", fexten, '/', sep="")
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
  cat('\n\n# =================================================#', file = paste("camcon_", rfile, sep=""), append=T)
  cat('\n# PARAMS: BLANK FOR NOW, CAN PUT ANYTHING HERE', file = paste("camcon_", rfile, sep=""), append=T)
  cat('\n# =================================================#', file = paste("camcon_", rfile, sep=""), append=T)
  param.names <- rep(NA, length(allparams))
  for(i in 1:length(allparams)) {
    param.names[i] <- createparam(allparams[i], ngrps, rfile)
  }
  param.names[!is.na(param.names)]
}

createparam <- function(param, ngrps, rfile) {
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
    if(length(par) == ngrps) cat('\n',param, sep='', file = paste("camcon_", rfile, sep=""), append=T)
    if(length(par) > ngrps) cat('\n',param,'[1:',ngrps,']', sep='', file = paste("camcon_", rfile, sep=""), append=T)
    if(length(par) < ngrps) cat('\n',lhs,' <- unlist(sapply(1:',ngrps,', function(i) ',rhs,'))[1:',ngrps,']',sep='', file = paste("camcon_", rfile, sep=""), append=T)
  }
  if(exists('lhs')) lhs
  else NA
}

dpreplace <- function(qscript, data, params) {
  for(i in 1:length(data)) {
    qscript <- gsub(data[i], paste(data[i],".ls[[i]]", sep=""), qscript)
  }
  for(i in 1:length(params)) {
    qscript <- gsub(params[i], paste(params[i],"[[i]]", sep=""), qscript)
  }
  qscript[qscript != '']
}

valid_var_names <- function(x, allow_reserved = TRUE, unique = FALSE) {
  ok <- rep.int(TRUE, length(x))
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(x) > max_name_length] <- FALSE
  if(!allow_reserved) {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }
  ok[x != make.names(x, unique = unique)] <- FALSE
  ok
}

qnamestovars <- function(qnames) {
  qnames <- sub("#", "", qnames)
  qnames <- sub("-", "_", qnames)
  qnames <- sub(" ", "", qnames)
  tolower(qnames)
}

getqlocs <- function(script, qpattern) {
  qlocs <- grep(qpattern, script)
  qnames <- qnamestovars(script[qlocs])
  validqs <- valid_var_names(qnames)
  if (FALSE %in% validqs) stop('invalid question name(s)')
  names(qlocs) <- qnames
  qlocs
}

createqs <- function(qscript, qlocs, ngrps, rfile) {
  text <- ''; for(i in 1:length(qscript)) text <- paste(text, '  ', qscript[i], '\n', sep='')
  qnames <- names(qlocs)
  qvec <- 'c('; for(i in 1:length(qnames)) qvec <- paste(qvec, qnames[i], ',', sep='')
  qvec <- paste(substr(qvec, 1, nchar(qvec)-1),')',sep='')
  qexist <- 'c('; for(i in 1:length(qnames)) qexist <- paste(qexist, '"', qnames[i], '",', sep='')
  qexist <- paste(substr(qexist, 1, nchar(qexist)-1),')',sep='')
  cat('\n\n# =================================================#',
      '\n# QUESTIONS: BLANK FOR NOW, CAN PUT ANYTHING HERE',
      '\n# =================================================#',
      '\ncamcon_sols <- list()\n',
      'for(i in 1:',nGrps,') {\n',
      text,
      '\n  if(i == 1) {\n',
      '    camcon_validq <- ', qexist, '\n',
      '    for(j in 1:length(camcon_validq)) if(!exists(camcon_validq[j])) stop(paste("INCONSISTENT VARIABLE NAME:",camcon_validq[j]))\n',
      '  }\n\n',
      '  camcon_sols[[i]] <- ', qvec, '\n',
      '  rm(list=camcon_validq)\n',
      '}', sep='', file = paste("camcon_", rfile, sep=""), append=T)
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
      if(unique(nchar(do.call(rbind, camcon_sols)[,1])) == 1) qtype[i] <- 'multiple'
      else qtype[i] <- 'text'
    }
  }
  qtype
}

getqoptions <- function(qlocs, qtype) {
  qnamesopts <- paste(names(qlocs),'_options',sep='')
  qoptions <- rep('', length(qlocs))
  for(i in which(qtype == 'multiple')) {
    qoptions[i] <- eval(parse(text=qnamesopts[i]))
  }
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

pnganswers <- function(qscript, qlocs, ngrps, rfile) {
  require(PerformanceAnalytics)
  fexten <- sub(' ', '', rfile)
  if(length(grep('\\.', fexten)) > 0) fexten <- unlist(strsplit(fexten,'\\.'))[1]
  root <- paste(getwd(), "/camcon_", fexten, '/', 'pngs/', sep="")
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

answercsv <- function(qloc, qtype, qoptions, qpoints, ngrps, rfile) {
  qnames <- sub('q','',names(qloc))
  qnames <- sub('_','-',qnames)
  fexten <- sub(' ', '', rfile)
  if(length(grep('\\.', fexten)) > 0) fexten <- unlist(strsplit(fexten,'\\.'))[1]
  root <- paste(getwd(), "/camcon_", fexten, '/', sep="")
  sol.mat <- c()
  for(i in 1:ngrps) {
    for(j in 1:length(qnames)) {
      sol.mat <- rbind(sol.mat, cbind(qnames[j], qtype[j], i, 0, qpoints[j], camcon_sols[[i]][j], qoptions[j], paste('sols_G', i, '_Q', j, '.png', sep='')))
    }
  }
  write.table(sol.mat, file = paste(root, 'sols_upload.csv', sep=''), append=T, col.names=F, row.names=F, sep=',')
}

tidyup <- function(rfile) {
  file.remove(paste(getwd(), "/camcon_", rfile, sep=""))
  rm(list=ls(envir=globalenv()), envir = globalenv())
}

# Point to R file
filename <- "mba.R"

# Specify Number of Groups
nGrps <- 10

# Run script
camcon(filename, nGrps, strpattern = c('#DATA','#PARAMS','#SCRIPT','#END'), qpattern = '#Q')