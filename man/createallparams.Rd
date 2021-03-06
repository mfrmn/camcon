\name{createallparams}
\alias{createallparams}
\title{Loops through parameter creation}
\usage{
  createallparams(allparams, ngrps, cc_rfile)
}
\arguments{
  \item{allparams}{list generated by getparams() of all
  lines expected to contain parameters}

  \item{ngrps}{number of unique solution sets to produce}

  \item{cc_rfile}{location of output file to be written to
  during replication process}
}
\description{
  Main parameter creation function that loops through each
  line given by getparams() and enlarges the parameter list
  so there is a unique value for each group.
}

