\name{gen_csv}
\alias{gen_csv}
\title{Main function for generating .csv file containing solutions}
\usage{
  gen_csv(ngrps, cc_dir, qlocs, qparams, pngs)
}
\arguments{
  \item{ngrps}{number of unique solution sets to produce}

  \item{cc_dir}{location of output directory to be written
  to during replication process}

  \item{qlocs}{locations in qscript of starting points of
  each question and question names}

  \item{qparams}{list of parameters for each question
  produced by function getqparams()}

  \item{pngs}{if T then produces .png solutions unique to
  each group for upload to camcon.eu}
}
\description{
  Calls all of the intermediary functions needed to
  generate the .csv that is uplodaded to camcon
}

