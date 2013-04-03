\name{valid_strpattern}
\alias{valid_strpattern}
\title{Check for valid structure identifiers}
\usage{
  valid_strpattern(strpattern, script)
}
\arguments{
  \item{strpattern}{pattern associated with data,
  parameter, and script sections in rfile}
}
\description{
  Throws error if structure identifier (i) is not
  specified, (ii) does not start with a hash, or (iii) none
  / multiple occurrences are identified in script.
}

