\name{getmaxlines}
\alias{getmaxlines}
\title{Determines the maximum of lines to allow for a text answer}
\usage{
  getmaxlines(qlocs, qparams, qtype)
}
\arguments{
  \item{qlocs}{locations in qscript of starting points of
  each question and question names}

  \item{qparams}{list of parameters for each question
  produced by function getqparams()}

  \item{qtype}{the type of question (e.g. numeric, text,
  ...) as determined by getqtype()}
}
\description{
  For each text question it ensures that a parameter equal
  to the maximum number of lines to allow is provided
}

