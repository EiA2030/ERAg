% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_outcome_ratio.R
\name{derive_outcome_ratio}
\alias{derive_outcome_ratio}
\title{derive_outcome_ratio
Calculates new outcomes by dividing one outcome from another. If function returns NULL then no derived data are present. Any observation outcome sets that already have a result outcome associated with them are removed.}
\usage{
derive_outcome_ratio(Data, numerator, denominator, result)
}
\arguments{
\item{Data}{An ERA data.table (e.g. \code{ERAg::ERA.Compiled}).}

\item{numerator}{ERA outcome code of numerator outcome (see \code{ERAg::OutcomeCodes})}

\item{denominator}{ERA outcome code of denominator outcome (see \code{ERAg::OutcomeCodes})}

\item{result}{ERA outcome code for the resulting values (see \code{ERAg::OutcomeCodes})}
}
\description{
derive_outcome_ratio
Calculates new outcomes by dividing one outcome from another. If function returns NULL then no derived data are present. Any observation outcome sets that already have a result outcome associated with them are removed.
}
