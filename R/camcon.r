#' Generates file for upload to camcon.eu
#'
#' This function takes a suitable .R input file and (optionally) .tex question sheet
#' and produces from this unique datasets, calculates individual answers, and
#' (optionally) creates .png solutions and individual question sheets for direct
#' upload to camcon.eu.
#'
#' @param rfile the R file for replication, must have specific structure (see documentation)
#' @param texqfile optional file to produce unique question sheets, must have specific structure (see documentation)
#' @param texafile optional file to produce unique answer sheets, must have specific structure (see documentation)
#' @param ngrps number of unique solution sets to produce
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
#' @param qpattern pattern to identify location of each unique question
#' @param pngs if T then produces .png solutions unique to each group for upload to camcon.eu
#' @param zip if T then zips all folders created by camcon ready for direct upload to camcon.eu
#' @param debug if T then post-processing does not occur, i.e. does not delete variables or intermediary files used by camcon()
#' @export
camcon <- function(rfile, texqfile = "", texafile = "", ngrps = 1, strpattern = c('#DATA','#PARAMS','#SCRIPT','#END'), qpattern = '#Q', pngs = T, zip=T, debug = F) {
  require(stringr); require(tools)
  origFiles <- ls(envir=globalenv())
  con <- file(rfile); rscript <- readLines(con, warn=F); close(con)
  if(texqfile != "") { con <- file(texqfile); qscript <- readLines(con, warn=F); close(con) }
  if(texafile != "") { con <- file(texafile); ascript <- readLines(con, warn=F); close(con) }
  valid_params(ngrps, strpattern, qpattern, pngs, debug, rscript)
  struc <- camcon_init(rfile)
  gen_out <- gen_script(ngrps, strpattern, qpattern, rscript, struc)
  source(struc$file)
  gen_csv(ngrps, struc$dir, gen_out$qlocs, gen_out$qparams, pngs)
  if(texqfile != "") gen_qpdfs(texqfile, ngrps, struc$dir, qscript, gen_out$params)
  if(pngs) pnganswers(gen_out$orig_qsection, gen_out$qlocs, ngrps, struc$dir)
  if(zip) {
    if(.Platform$OS.type == "unix") {
      system(paste('zip -rj "',struc$dir,'upload.zip" "',struc$dir,'output/"', sep=""), ignore.stdout=T)
      for(i in 1:ngrps) {
        system(paste('zip -rj "',struc$dir,'data_',i,'.zip" "',struc$dir,'data_',i,'/"', sep=""), ignore.stdout=T)
      }
      unlink(paste(struc$dir,list.files(path=struc$dir)[!(list.files(path=struc$dir) %in% list.files(path=struc$dir, pattern="\\.zip"))],sep=""), recursive=T)
    }
  }
  if(!debug) tidyup(struc$file, origFiles)
  message(paste('Camcon output placed in folder:',struc$dir,'\n--------------'))
}

tidyup <- function(cc_rfile, origFiles) {
  file.remove(cc_rfile)
  rm(list=ls(envir=globalenv())[!(ls(envir=globalenv()) %in% origFiles)], envir = globalenv())
}
