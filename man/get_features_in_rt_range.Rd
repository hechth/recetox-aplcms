% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recover.weaker.R
\name{get_features_in_rt_range}
\alias{get_features_in_rt_range}
\title{Get peaks and valleys of smoothed rt values in range.}
\usage{
get_features_in_rt_range(features, times, bw)
}
\arguments{
\item{features}{tibble Data table with `rt` and `intensities` columns.}

\item{times}{vector Raw retention time data from raw data file.}

\item{bw}{float Bandwidth to use for kernel smoothing.}
}
\value{

}
\description{
Get peaks and valleys of smoothed rt values in range.
}
