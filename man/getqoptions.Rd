\name{getqoptions}
\alias{getqoptions}
\title{Determines the number of valid options to display for a single/multiple choice question}
\usage{
  getqoptions(qlocs, qparams, qtype)
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
  For each single/multiple choice question it ensures that
  a parameter equal to the number of options to display is
  provided and that these are sufficient based on the
  generated solutions
}

