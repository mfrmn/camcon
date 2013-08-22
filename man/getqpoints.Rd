\name{getqpoints}
\alias{getqpoints}
\title{Sets the number of points that each question is worth}
\usage{
  getqpoints(qlocs, qparams)
}
\arguments{
  \item{qlocs}{locations in qscript of starting points of
  each question and question names}

  \item{qparams}{list of parameters for each question
  produced by function getqparams()}
}
\description{
  For each question it first detects whether the user has
  specified the number of points that the question is worth
  in the parameters, and if not assigns points so that the
  total number of points available equals 100
}

