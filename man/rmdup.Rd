% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_dup.R
\name{rmdup}
\alias{rmdup}
\title{Removing Duplicated Tests Function}
\usage{
rmdup(data, examName)
}
\arguments{
\item{data}{a dataframe for scoring}

\item{examName}{Exam name i.e. InternalMed, PCC, Peds...}
}
\value{
a dataframe with exam name attached to it
}
\description{
This function allows you to remove students with more than one test on the same cases within same exam and only keep the first time score
}
