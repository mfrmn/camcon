\name{camcon}
\alias{camcon}
\title{Generates file for upload to camcon.eu}
\usage{
  camcon(rfile, texqfile = "", texafile = "", ngrps = 1,
    strpattern = c("#DATA", "#PARAMS", "#SCRIPT", "#END"),
    qpattern = "#Q", pngs = T, debug = F)
}
\arguments{
  \item{rfile}{the R file for replication, must have
  specific structure (see documentation)}

  \item{texqfile}{optional file to produce unique question
  sheets, must have specific structure (see documentation)}

  \item{texafile}{optional file to produce unique answer
  sheets, must have specific structure (see documentation)}

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
}
\description{
  This function takes a suitable .R input file and
  (optionally) .tex question sheet and produces from this
  unique datasets, calculates individual answers, and
  (optionally) creates .png solutions and individual
  question sheets for direct upload to camcon.eu.
}

