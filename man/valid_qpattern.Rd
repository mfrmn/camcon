\name{valid_qpattern}
\alias{valid_qpattern}
\title{Check for valid question identifier}
\usage{
  valid_qpattern(qpattern, rscript)
}
\arguments{
  \item{strpattern}{pattern associated with data,
  parameter, and script sections in rfile}

  \item{rscript}{line by line list produced from R file for
  replication}
}
\description{
  Throws error if question identifier (i) is not specified,
  (ii) does not start with a hash, or (iii) no occurrences
  are identified in script.
}

