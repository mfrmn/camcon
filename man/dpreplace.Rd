\name{dpreplace}
\alias{dpreplace}
\title{Replaces original data and parameters in script with new versions unique to each individual}
\usage{
  dpreplace(qsection, data, params)
}
\arguments{
  \item{qsection}{line by line list produced from question
  section of the original R file}

  \item{data}{the names of all of the detected data frames}

  \item{params}{the names of all of the detected
  parameters}
}
\description{
  Goes through the question part of the script and every
  time it finds a match with one of the names of a data
  frame or a parameter vector specified in the #DATA and
  #PARAMS sections it replaces it with the a new version
  which was generated using createalldata() and
  createallparams()
}

