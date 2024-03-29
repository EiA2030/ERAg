% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StabCalc2.R
\name{StabCalc2}
\alias{StabCalc2}
\title{Calculate Outcome Stability For Each Element of Practice x Outcome}
\usage{
StabCalc2(
  Data,
  Do.Weight = T,
  Weight.by.Study = T,
  Rm.Out = T,
  Transform = T,
  DoRandom = T,
  Control = list(optimizer = "optim", optmethod = "Nelder-Mead", maxit = 10000),
  Responses = c("lnRR", "lnVR", "lnCVR"),
  Use.acv = F,
  Inc.Prod = F,
  Cores = 1
)
}
\arguments{
\item{Data}{A data.table output by the \code{ERAg::PrepareStabData} function}

\item{Do.Weight}{logical, if \code{TRUE} coefficient estimates are weighted acccording to the supplied weightings in the \code{Data} object supplied(default = T)}

\item{Weight.by.Study}{logical, depreciated (default = \code{T})}

\item{Rm.Out}{logical, if \code{TRUE} extreme outliers are removed withing each Practice x Outcome combination as per the method detailed in \code{ERAg::OutCalc} (default = \code{T})}

\item{Transform}{logical, if \code{TRUE} back-transformed coefficient estimates and confidence intervals are appended to outputs  (default = \code{T})}

\item{Control}{list, optional list of control values for the \code{rma.mv} estimation algorithms. If unspecified, default values are defined inside the function (default = \code{list(optimizer="optim",optmethod="Nelder-Mead",maxit=10000)})}

\item{Responses}{character vector, this argument is depreciated do edit (default=\code{c("lnRR","lnVR","lnCVR")})}

\item{Use.acv}{logical T/F. If \code{T} scale-adjusted coefficient of variation, acv, is substituted for the coefficient of variation (cv).}

\item{Inc.Prod}{logical T/F. If \code{F} analysis is conducted at the practice x outcome level (i.e. aggregated across crops). If \code{T} analysis is dis-aggregated to the Crop x Outcome x Practice level (Crops represented by the \code{EU} column in the \verb{Data parameter})}

\item{Cores}{integer. The number of cores to use for parallel processing, if set to \code{1} no parallel processing is applied.}
}
\value{
\code{StabCalc2} returns a \code{list}
\enumerate{
\item \strong{\verb{[[StabStats]]}} The first level of this list contains the outputs of \code{ERAg::StabCalc} for each Practice x Outcome ( x EU if Inc.Prod==T) combination present in \code{Data}
\item \strong{\verb{[[StabStats.Test]]}} This is the equivalent of the \strong{\verb{[[Tests]]}} \code{data.table} output by \code{ERAg::StabCalc}
\item \strong{\verb{[[StabStats.Tab]]}} This is the equivalent of the \strong{\verb{[[Coefs]]}} \code{data.table} output by \code{ERAg::StabCalc}
\item \strong{\verb{[[StabStats.Test2]]}} This is the equivalent of the \strong{\verb{[[Tests2]]}} \code{data.table} output by \code{ERAg::StabCalc}
\item \strong{\verb{[[StabList]]}} A \code{list} of the input \code{Data} \code{data.table} split by Practice and Outcome ( and EU if Inc.Prod==T)
}
}
\description{
\code{StabCalc2}runs splits the \code{data.table} output by \code{PrepareStabData} into a list using the \code{Outcome} & \code{Practice} fields then applies \code{ERAg::StabCalc}
to each element of the list. The output tables are bound backtogether and output in a list.
}
