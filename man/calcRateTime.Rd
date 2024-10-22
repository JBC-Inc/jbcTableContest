% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{calcRateTime}
\alias{calcRateTime}
\title{Calculate Rate Time}
\usage{
calcRateTime(filtered, production, nll = 10000)
}
\arguments{
\item{filtered}{filtered subset of the SL dataset.}

\item{production}{production data.}

\item{nll}{reactive normalized lateral length.}
}
\value{
\code{calcRateTime} returns a list of:
\itemize{
\item \verb{raw rate-time                well level production}
\item \verb{scaled rate-time             well level production}
\item \verb{aggregated rate-time        group level production}
\item \verb{scaled aggregated rate-time group level production}
}
}
\description{
Selected filtered wells, combine production data (decline rate) and calculate new output
}
