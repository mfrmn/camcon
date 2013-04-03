\name{getdata}
\alias{getdata}
\title{Collects data frames}
\usage{
  getdata(script, start, end)
}
\arguments{
  \item{script}{line by line list produced from R file for
  replication}

  \item{start}{an integer specifying the index value in
  script at which the data section starts}

  \item{end}{an integer specifying the index value in
  script at which the data section ends}
}
\description{
  Outputs list of all data frames specified in data section
}

