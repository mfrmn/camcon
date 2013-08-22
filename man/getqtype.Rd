\name{getqtype}
\alias{getqtype}
\title{Tries to automatically determine question type and checks against any supplied}
\usage{
  getqtype(ngrps, qlocs, qparams)
}
\arguments{
  \item{ngrps}{number of unique solution sets to produce}

  \item{qlocs}{locations in qscript of starting points of
  each question and question names}

  \item{qparams}{list of parameters for each question
  produced by function getqparams()}
}
\description{
  Goes through the solutions and looks for particular
  characteristics (all numeric, all single alphabetical
  character solutions e.g. A, B, C, otherwise), and then
  guesses the question type based on this. It then compares
  this against the supplied question type.
}

