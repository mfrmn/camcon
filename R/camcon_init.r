#' Creates folder for camcon files and blank script
#'
#' Creates folder /camcon_filename/ for storing data files, question sheets,
#' .csv file with solutions, and .png files, and blank script for camcon
#' replication.
#'
#' @param rfile name of the R file being replicated through camcon
camcon_init <- function(rfile) {
  fexten <- gsub(' ', '', rfile)
  if(length(grep('/', fexten)) > 0) fexten <- unlist(strsplit(fexten,'/'))[length(unlist(strsplit(fexten,'/')))]
  if(length(grep('\\.', fexten)) > 0) fexten <- unlist(strsplit(fexten,'\\.'))[1]
  fexten <- gsub("[[:punct:]]","",fexten)
  
  camcon_root <- paste(getwd(), "/camcon_", fexten, '/', sep="")
  if(file.exists(camcon_root)) unlink(camcon_root, recursive=T) # delete folder if already exists
  dir.create(camcon_root)
  
  camcon_file <- paste("camcon_", fexten, '.R', sep="")
  if(file.exists(camcon_file)) file.remove(camcon_file) # delete file if already exists
  file.create(camcon_file)
  
  list(dir=camcon_root, file=camcon_file)
}


