#' Outputs header information to file
#'
#' Writes all lines from the start of the script up until the specified end location
#' to a new file.
#'
#' @param rscript line by line list produced from R file for replication
#' @param cc_rfile location of output file to be written to during replication process
#' @param end an integer specifying the index value in script at which the header ends
makeheader <- function(rscript, cc_rfile, end) {
  cat('# =================================================#',
      '\n# HEADER: ALL REQ. PACKAGES AND GENERAL FUNCTIONS',
      '\n# =================================================#\n', file = cc_rfile, append=T)
  for (i in 1:end) {
    write(rscript[i], file = cc_rfile, append=T)
  }
}