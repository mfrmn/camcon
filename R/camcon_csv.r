all.is.numeric <- function (x) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  !any(is.na(suppressWarnings(as.numeric(x))))
}

getqtype <- function(ngrps, qlocs, qparams) {
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
    if(qparams[[i]]$type != "") {
      if(qtype[i] == "number" & qparams[[i]]$type != "NUM") stop(paste("Detected numeric answers for ",names(qlocs)[i]," but NUM not specified in question parameters.", sep=""))
      if(qtype[i] == "multiple" & !(qparams[[i]]$type %in% c("SC", "MC"))) stop(paste("Detected single/multiple choice answers for ",names(qlocs)[i]," but MC not specified in question parameters.", sep=""))
      if(qtype[i] == "multiple" & qparams[[i]]$type == "SC") qtype[i] <- "single"
      if(qtype[i] == "text" & qparams[[i]]$type != "TXT") stop(paste("Detected text for ",names(qlocs)[i]," but TXT not specified in question parameters.", sep=""))
    }
  }
  qtype
}

getqoptions <- function(qlocs, qparams, qtype) {
  qoptions <- rep('', length(qlocs))
  mchoice <- which(qtype %in% c('single','multiple'))
  if(length(mchoice) > 0) {
    mcopts <- unlist(lapply(qparams, function(x) x$opts))[mchoice]
    if(length(which(mcopts == "")) > 0) stop(paste('Please specify number of options for single/multiple choice question(s):\n    ', paste(names(qlocs)[mchoice][which(mcopts == "")], collapse=', ')))
    qoptions[mchoice] <- mcopts
  }
  for(i in mchoice) {
    validoption <- tolower(unlist(lapply(camcon_sols, function(x) x[i]))) %in% letters[1:qoptions[i]]
    if(FALSE %in% validoption)  stop(paste('You have specified ',qoptions[i],' options for single/multiple choice question ',names(qlocs[i]), ' but we detected solutions outside of this range', sep=""))
  }
  qoptions
}

getmaxlines <- function(qlocs, qparams, qtype) {
  qoptions <- rep('', length(qlocs))
  istext <- which(qtype == 'text')
  if(length(istext) > 0) {
    nlines <- unlist(lapply(qparams, function(x) x$opts))[istext]
    if(length(which(nlines == "")) > 0) stop(paste('Please specify max number of lines for text question(s):\n    ', paste(names(qlocs)[istext][which(nlines == "")], collapse=', ')))
    qoptions[istext] <- nlines
  }
  qoptions
}

getqpoints <- function(qlocs, qparams) {
  totpoints <- 100
  finalpoints <- unlist(lapply(qparams, function(x) x$points))
  emptypoints <- finalpoints[finalpoints == ""]
  if(sum(finalpoints == "") != 0) {
    totpoints <- totpoints - sum(as.numeric(finalpoints[finalpoints != ""]))
    if(totpoints > 0) {
      nqs <- length(emptypoints)
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
      warning(paste("You did not specify the number of points to assign for the question(s):\n    ",paste(names(qlocs)[which(finalpoints == "")],collapse=", "),"\nand so points were assigned automatically under the assumption that the desired total = 100. This may result in undesirable distribution of points.",sep=""), call. = FALSE)
      finalpoints[finalpoints == ""] <- points
    }
    else {
      warning(paste("You did not specify the number of points to assign for the question(s):\n    ",paste(names(qlocs)[which(finalpoints == "")],collapse=", "),"\nbut the total sum of points already equals or exceeds the desired total = 100 and so 0 weight was assigned to this/these question(s).",sep=""), call. = FALSE)
      finalpoints[finalpoints == ""] <- 0
    }
  }
  finalpoints
}

answercsv <- function(qlocs, qtype, qoptions, qmaxlines, qpoints, ngrps, cc_dir) {
  qnames <- gsub('q','',names(qlocs))
  qnames <- gsub('_','-',qnames)
  sol.mat <- c()
  for(i in 1:ngrps) {
    for(j in 1:length(qnames)) {
      soln <- camcon_sols[[i]][j]
      if(qtype[j] == "text") soln <- qmaxlines[j]
      sol.mat <- rbind(sol.mat, cbind(qnames[j], qtype[j], i, 0, qpoints[j], soln, qoptions[j], paste('sols_G', i, '_Q', j, '.png', sep='')))
    }
  }
  ul_folder <- paste(cc_dir, "output/", sep="")
  if(!file.exists(ul_folder)) dir.create(ul_folder)
  write.table(sol.mat, file = paste(ul_folder, 'sols_upload.csv', sep=''), append=T, col.names=F, row.names=F, sep=',')
}

gen_csv <- function(ngrps, cc_dir, qlocs, qparams) {
  qtype <- getqtype(ngrps, qlocs, qparams)
  qoptions <- getqoptions(qlocs, qparams, qtype)
  qmaxlines <- getmaxlines(qlocs, qparams, qtype)
  qpoints <- getqpoints(qlocs, qparams)
  answercsv(qlocs, qtype, qoptions, qmaxlines, qpoints, ngrps, cc_dir)
}