\name{gen_qpdfs}
\alias{gen_qpdfs}
\title{Generates script used by camcon function to produce unique datasets and solutions.}
\usage{
  gen_qpdfs(texqfile, ngrps, cc_dir, qscript, params)
}
\arguments{
  \item{ngrps}{number of unique solution sets to produce}

  \item{params}{the names of all of the detected
  parameters}
}
\description{
  This function takes a suitable .R input file and
  (optionally) .tex question sheet and produces from this
  unique datasets, calculates individual answers, and
  (optionally) creates .png solutions and individual
  question sheets for direct upload to camcon.eu.
}

