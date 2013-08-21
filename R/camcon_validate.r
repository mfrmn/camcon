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
#' @param rscript line by line list produced from R file for replication
valid_params <- function(ngrps, strpattern, qpattern, pngs, debug, rscript) {
  valid_grp_num(ngrps)
  valid_strpattern(strpattern, rscript)
  valid_qpattern(qpattern, rscript)
  if(!is.logical(pngs)) stop('Parameter "pngs" should be logical (i.e. TRUE or FALSE)')
  if(!is.logical(debug)) stop('Parameter "debug" should be logical (i.e. TRUE or FALSE)')
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
#' with a hash, (iii) none / multiple occurrences are identified in script,
#' or (iv) order of appearance in script is incorrect
#'
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
#' @param rscript line by line list produced from R file for replication
valid_strpattern <- function(strpattern, rscript) {
  if(length(strpattern) != 4) stop('Invalid length for parameter strpattern. Expected four entries.')
  hash_start <- (regexpr('#', strpattern) == 1)
  if(FALSE %in% hash_start) stop(paste('Each element in parameter strpattern must begin with a hash (#). Fix:\n    ', paste(strpattern[!hash_start], collapse=', ')))
  uniq_strpattern <- lapply(sapply(strpattern, function(i) grep(i, rscript)), length) == 1
  if(FALSE %in% uniq_strpattern) stop(paste('Found none or multiple occurrences of element(s) of strpattern in script. Each must only appear once. Fix:\n    ', paste(strpattern[!uniq_strpattern], collapse=', ')))
  loc_increasing <- sapply(strpattern, function(i) grep(i, rscript)); loc_increasing <- (loc_increasing[-1]-loc_increasing[-4] > 0)
  if(FALSE %in% loc_increasing) stop(paste('Elements in parameter strpattern do not appear in the R script in the correct order.'))
}

#' Check for valid question identifier
#'
#' Throws error if question identifier (i) is not specified, (ii) does not start
#' with a hash, or (iii) no occurrences are identified in script.
#'
#' @param strpattern pattern associated with data, parameter, and script sections in rfile
#' @param rscript line by line list produced from R file for replication
valid_qpattern <- function(qpattern, rscript) {
  if(length(qpattern) != 1) stop('Invalid length for parameter qpattern. Expected one entry')
  if(regexpr('#', qpattern) != 1) stop('Parameter qpattern must begin with a hash (#)')
  if(length(grep(qpattern, rscript)) == 0) stop('Found no occurrences of qpattern in script')
}