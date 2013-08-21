\name{getparams}
\alias{getparams}
\title{Collects parameter section}
\usage{
  getparams(script, start, end)
}
\arguments{
  \item{rscript}{line by line list produced from R file for
  replication}

  \item{start}{an integer specifying the index value in
  script at which the data section starts}

  \item{end}{an integer specifying the index value in
  script at which the data section ends}
}
\description{
  Outputs list of all non-empty lines specified in
  parameter section
}

