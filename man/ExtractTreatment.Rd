% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExtractTreatment.R
\name{ExtractTreatment}
\alias{ExtractTreatment}
\title{Determine Practice Set Differences and Commonalities}
\usage{
ExtractTreatment(Data, cores, N.Cols)
}
\arguments{
\item{Data}{A raw ERA dataset.}

\item{cores}{The number of logical cores to use for parallel processing.}

\item{N.Cols}{The number of T or C columns in the dataset.}
}
\value{
Two columns are appended to \code{Data}:
\enumerate{
\item \code{plist} = a \strong{list} of practice codes in the experimental treatment not in the control treatment.
\item \code{base.list} = a \strong{vector} of practice codes shared by the experimental and control treatments, codes are concatenated with a \code{-} delimiter.
}
}
\description{
This function takes a raw ERA dataset and determines set differences (practices in the experimental but not the control treatment) and
commonalities (practices shared by experimental and control treatments) for each row of the data. Control \code{h} codes are removed from the comparison.
}
\details{
In the raw dataset treatment practice codes are recorded in \code{T} columns (e.g., \code{T1},\code{T2}, etc.) for the experimental condition and \code{C} columns
for the control condition.
}
