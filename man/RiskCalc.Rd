% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RiskCalc.R
\name{RiskCalc}
\alias{RiskCalc}
\title{Calculate Risk}
\usage{
RiskCalc(Data, PLevel = "PrName", Out.Codes = 101, MinYear = 3)
}
\arguments{
\item{Data}{A prepared (ERA) dataset, see the \code{ERA.Prepare} function.}

\item{PLevel}{The column name of the variable in \code{Data} that describes the practice. Use this parameter to choose different levels of the practice hierarchy. Default = \code{PrName} (Practice).}

\item{Out.Codes}{A vector of outcome codes to consider in the analysis. Default = \code{101} (Crop Yield).}

\item{MinYear}{An integer value for the minimum length of a MYO sequence. Sequences with fewer growing season than this number are excluded from analysis. Default = \code{3}.}
}
\value{
A list of two data.tables \code{Risk} and \code{Risk.Averages}.
\code{Risk} contains statistics calculated for each value of \code{UID} and has the fields:
\itemize{
\item \code{UID} = a unique identifier based on the field \code{Outcome},\code{Practice},\code{Practice.Base},\code{Practice.Code},\code{Code},\code{ID},\code{Site.ID},\code{EU},\code{T.Descrip},\code{C.Descrip},\code{T.NI},\code{T.NO},\code{C.NI},\code{C.NO},\code{Tree},\code{Variety},\code{Diversity}, and \code{Rep}.
\item \code{N.Years}= the number of unique growing seasons reported for each value of \code{UID}.
\item \code{N.Obs} = the total number of observation for a value of \code{UID}.
\item \code{Diff.Mean} = mean difference between experimental and control treatments (\code{mean(MeanT-MeanC)})
\item \code{Diff.SD} = standard deviation of mean difference (\code{sd(MeanT-MeanC)})
\item \code{Diff.t.stat} = t-statistic of  mean difference (\code{.Mean/(.SD/N.Obs^0.5)})
\item \code{Diff.p.val} = probability of  mean difference being <0   (\code{pt(Diff.t.stat,N.Obs-1,lower.tail = T)})
\item \code{Mean.C} = mean of control treatment
\item \code{Mean.T} = mean of experimental treatment
\item \code{Mean.T.SD} = standard deviation of experimental treatment
\item \code{Mean.t.stat} = t-statistic for \code{MeanT} < \code{MeanC} (\verb{Mean.T-Mean.C)/(Mean.T.SD/N.Obs^0.5})
\item \code{Mean.p.val} = probability of \code{MeanT} < \code{MeanC}   (\code{pt(Mean.t.stat,N.Obs-1,lower.tail = T)})
\item \code{N.Obs.Study} = number of observations a study (\code{Code} column) contributes to a combination of practice x outcome
}

\code{Risk.Averages} is the data in the \code{Risk} table averaged with weighting across practice x outcome combinations. Additional fields are:
\itemize{
\item \code{Mean.Seq.Len}= the mean value of \code{Risk$N.Years}
\item \code{Median.Seq.Len}= the median value of \code{Risk$N.Years}
\item \code{N.Studies} = the number of studies contributing MYOs
\item \code{Total.Obs}= the total number of observations contributing to MYOs
\item \code{N.Obs}= the total number of MYOs
\item \code{Diff.p.val.se} = the standard error of \code{Risk$Diff.p.val}
\item \code{Mean.p.val.se} = the standard error of \code{Risk$Mean.p.val}
\item \code{Diff.CI95low} = lower 95\% confidence interval of \code{Diff.p.val}
\item \code{Diff.CI95high} = upper 95\% confidence interval of \code{Diff.p.val}
\item \code{Mean.CI95low} = lower 95\% confidence interval of \code{Mean.p.val}
\item \code{Mean.CI95high} = upper 95\% confidence interval of \code{Mean.p.val}
}
}
\description{
This function estimates the production risk of an experimental treatment compared to the control treatment using time series data.  We suggest applying the \code{ERA.Prepare} function to
data before using with this function.
}
\details{
Method:
We adapted a lower confidence limit (LCL) approach (e.g.  \href{https://www.cabdirect.org/cabdirect/abstract/19996775299}{Hildebrand1996},
\href{https://doi.org/10.1016/S0167-8809(00)00140-7}{Yamoah2000}, \href{https://doi.org/10.3763/ijas.2010.0471}{Sirrine2010}) to estimate production risk as:
\enumerate{
\item \code{Risk.Means} = the probability of the mean experimental treatment yielding lower than the control treatment; and
\item \code{Risk.Diff} = the probability of the mean yield difference between experimental and control treatments being less than 0.
}

A minimum of three seasons of yield data from the same experimental treatments are required to calculate risk and this is the default threshold for the analyses presented here.
The minimum number of season for a multi-year observation (MYO) to be included in analyses can be adjusted using the \code{MinYear} parameter.

Statistics are reported aggregated to the level of practice hierarchy specified in the \code{PLevel} parameter.

Outcomes are analyzed at the subindicator level. Whilst any outcome(s) can be used with this function, it's primary purpose is to analyze productivity outcomes such as crop yield or net returns.

Weightings are applied to mean and error estimates as per the methods described in the \code{ERA.Analyze} function.
}
