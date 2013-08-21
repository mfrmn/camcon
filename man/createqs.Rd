\name{createqs}
\alias{createqs}
\title{Writes question section to the camcon script}
\usage{
  createqs(qscript, qlocs, ngrps, cc_rfile)
}
\arguments{
  \item{qscript}{line by line list produced from question
  section of the original R file}

  \item{qlocs}{locations in qscript of starting points of
  each question and question names}

  \item{ngrps}{number of unique solution sets to produce}

  \item{cc_rfile}{location of output file to be written to
  during replication process}
}
\description{
  Creates the loop that goes through and creates unique
  solutions based on the individual data sets and
  parameters
}

