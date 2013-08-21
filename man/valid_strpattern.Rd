\name{valid_strpattern}
\alias{valid_strpattern}
\title{Check for valid structure identifiers}
\usage{
  valid_strpattern(strpattern, rscript)
}
\arguments{
  \item{strpattern}{pattern associated with data,
  parameter, and script sections in rfile}

  \item{rscript}{line by line list produced from R file for
  replication}
}
\description{
  Throws error if structure identifier (i) is not
  specified, (ii) does not start with a hash, (iii) none /
  multiple occurrences are identified in script, or (iv)
  order of appearance in script is incorrect
}

