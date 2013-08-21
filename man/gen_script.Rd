\name{gen_script}
\alias{gen_script}
\title{Generates script used by camcon function to produce unique datasets and solutions.}
\usage{
  gen_script(ngrps, strpattern, qpattern, rscript, struc)
}
\arguments{
  \item{ngrps}{number of unique solution sets to produce}

  \item{strpattern}{pattern associated with data,
  parameter, and script sections in rfile}

  \item{qpattern}{pattern to identify location of each
  unique question}

  \item{rscript}{line by line list produced from R file for
  replication}

  \item{struc}{location of output folder and file to be
  written to during replication process}
}
\description{
  This function takes a suitable .R input file and
  (optionally) .tex question sheet and produces from this
  unique datasets, calculates individual answers, and
  (optionally) creates .png solutions and individual
  question sheets for direct upload to camcon.eu.
}

