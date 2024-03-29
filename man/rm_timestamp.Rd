% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rm_timestamp.R
\name{rm_timestamp}
\alias{rm_timestamp}
\title{rm_timestamp}
\usage{
rm_timestamp(strings, sep = "_", timestamp_format = "\%Y-\%m-\%d_\%H.\%M.\%S")
}
\arguments{
\item{strings}{A vector of stings.}

\item{sep}{A string with the timestamp seperator.}

\item{timestamp_format}{A string with the timestamp format, as input for
as.POSIXct.}
}
\value{
A vecotr of strings with the timestamps removed.
}
\description{
rm_timestamp removes timestamps from strings.
}
