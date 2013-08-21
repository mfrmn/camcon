\name{valid_params}
\alias{valid_params}
\title{Check for valid initialising parameters in camcon() function}
\usage{
  valid_params(ngrps, strpattern, qpattern, pngs, debug,
    rscript)
}
\arguments{
  \item{ngrps}{number of unique solution sets to produce}

  \item{strpattern}{pattern associated with data,
  parameter, and script sections in rfile}

  \item{qpattern}{pattern to identify location of each
  unique question}

  \item{pngs}{if T then produces .png solutions unique to
  each group for upload to camcon.eu}

  \item{debug}{if T then post-processing does not occur,
  i.e. does not delete variables or intermediary files used
  by camcon()}

  \item{rscript}{line by line list produced from R file for
  replication}
}
\description{
  Throws error if invalid parameter given for ngrps,
  strpattern, qpattern pngs or debug.
}

