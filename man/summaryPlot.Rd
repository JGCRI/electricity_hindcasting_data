% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summaryPlot}
\alias{summaryPlot}
\title{Plot the summary time-series for variable of interest}
\usage{
summaryPlot(df, column, units)
}
\arguments{
\item{df}{data.frame, summary data produced by summaryCalc()}

\item{column}{unquoted column name}

\item{units}{string, Units of variable}
}
\value{
time-series plot faceted by oc & fg
}
\description{
Plot the summary time-series for variable of interest
}
