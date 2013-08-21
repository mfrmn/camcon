\name{qtovar}
\alias{qtovar}
\title{Attempts to convert a question name into a valid variable name}
\usage{
  qtovar(qname)
}
\arguments{
  \item{qname}{string to convert into a valid question
  name}
}
\description{
  Removes # and " " and replaces "-" with "_" before
  converting question name to lower case, and then checks
  whether is a valid variable name
}

