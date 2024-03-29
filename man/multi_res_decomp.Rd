% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_res_decomp.R
\name{multi_res_decomp}
\alias{multi_res_decomp}
\title{Multiresolution Decomposition}
\usage{
multi_res_decomp(df, col1, col2 = NULL, time_res = 0.2, inc_w_n = F)
}
\arguments{
\item{df}{Data.frame containing the timeseries to be decomposed}

\item{col1}{Name of first column}

\item{col2}{Optional name of second column, when supplied cospectra of col1*col2 is produced}

\item{time_res}{The current time resolution of the series in seconds.}

\item{inc_w_n}{Default FALSE, when TRUE the intersitial "w_n" steps are returned. In this case function returns a list}
}
\value{
data.frame of spectra
}
\description{
This function performs the actual decomposition, following the steps presented in Vickers and Marht 2003 \cr
\cr
A simple multiresolution spectra is produced when only col1 is defined, and a cospectra is produced if col2 is also defined
}
\author{
W. S. Drysdale
}
