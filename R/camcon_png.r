pnganswers <- function(qsection, qlocs, ngrps, cc_dir) {
  require(PerformanceAnalytics)
  root <- paste(cc_dir, 'output/', sep="")
  if(!file.exists(root)) dir.create(root)
  locs <- c(qlocs, length(qsection)+1)
  for(i in 1:ngrps) {
    for(j in 1:(length(locs)-1)) {
      basecharperline <- 75; basenlines <- 25; cexval <- 1.5
      charperline <- floor(basecharperline / cexval)
      nline <- floor(basenlines / cexval)
      text <- c(qsection[locs[j]:(locs[j+1]-1)],'',paste('#ANS:',camcon_sols[[i]][j]))
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
