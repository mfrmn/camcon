\name{getqlocs}
\alias{getqlocs}
\title{Collects starting position of each new question}
\usage{
  getqlocs(qscript, qpattern)
}
\arguments{
  \item{qscript}{line by line list produced from question
  section of the original R file}

  \item{qpattern}{pattern to identify location of each
  unique question}
}
\description{
  Outputs the location (line number) of every match of
  qpattern (default #Q) in the appropriate script
}

