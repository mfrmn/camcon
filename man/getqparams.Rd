\name{getqparams}
\alias{getqparams}
\title{Outputs points assigned to question, question type and associated parameter}
\usage{
  getqparams(qscript, qlocs)
}
\arguments{
  \item{qscript}{line by line list produced from question
  section of the original R file}

  \item{qlocs}{locations in qscript of starting points of
  each question and question names}
}
\description{
  Looks to ensure that required parameters are specified
  for each question and converts these parameters into a
  list
}

