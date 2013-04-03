\name{makeheader}
\alias{makeheader}
\title{Outputs header information to file}
\usage{
  makeheader(script, ccfile, end)
}
\arguments{
  \item{script}{line by line list produced from R file for
  replication}

  \item{ccfile}{name of output file to be written to during
  replication process}

  \item{end}{an integer specifying the index value in
  script at which the header ends}
}
\description{
  Writes all lines from the start of the script up until
  the specified end location to a new file.
}

