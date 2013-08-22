\name{answercsv}
\alias{answercsv}
\title{Produces a .csv file with the required structure to upload to camcon.eu}
\usage{
  answercsv(qlocs, qtype, qoptions, qmaxlines, qpoints,
    ngrps, cc_dir, pngs)
}
\arguments{
  \item{qlocs}{locations in qscript of starting points of
  each question and question names}

  \item{qtype}{the type of question (e.g. numeric, text,
  ...) as determined by getqtype()}

  \item{qoptions}{the number of options produced by
  getqoptions()}

  \item{qmaxlines}{the max number of lines produced by
  getmaxlines()}

  \item{qpoints}{the number of points assigned to each
  question produced by getqpoints()}

  \item{ngrps}{number of unique solution sets to produce}

  \item{cc_dir}{location of output directory to be written
  to during replication process}

  \item{pngs}{if T then produces .png solutions unique to
  each group for upload to camcon.eu}
}
\description{
  Takes all of the properties of each question and all of
  the unique solutions and produces a .csv file called
  sols_upload.csv that contains all of the information
  required for automatic upload to camcon.eu
}

