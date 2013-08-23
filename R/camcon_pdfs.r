gen_qpdfs <- function(texqfile, ngrps, cc_dir, qscript, params) {
  currdir <- getwd()
  path <- cc_dir
  if(length(grep("[[:space:]]",path))>0) {
    path <- substr(path, 1, regexpr("[[:space:]]", path)[1])
    path <- strsplit(path,"/")[[1]]
    path <- paste(paste(path[-length(path)],collapse="/"),"/",sep="")
  }
  filename <- paste(strsplit(basename(texqfile),"\\.")[[1]][1],".tex",sep="")
  for(i in 1:ngrps) {
    nqscript <- qscript
    setwd(paste(cc_dir,"data_",i,"/",sep=""))
    filepath <- paste(path,filename,sep="")
    if(sum(is.na(params)) == 0) {
      for(j in 1:length(params)) {
        nqscript <- gsub(params[j], eval(parse(text=params[j]))[i], nqscript)
      }
    }
    lapply(nqscript, write, filepath, append=TRUE, ncolumns=1000)
    texi2pdf(filepath, texinputs=dirname(texqfile))
    file.remove(filepath)
    file.remove(list.files(pattern="\\.aux|\\.log|\\.out"))
  }
  setwd(currdir)
}
