% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tags2hash}
\alias{tags2hash}
\title{Converts tags into a hash-tagged string}
\usage{
tags2hash(tags)
}
\arguments{
\item{tags}{A character vector of tags
(e.g., c("R", "Health data")).}
}
\value{
A single string with tags
converted to a hash-tagged format
(e.g., "#Rstats #HealthData").
Specific transformation applied:
- Converts "r" (case insensitive)
to "rstats".
- Removes spaces within tags.
- Collapses all elements into one
space-separated string.
}
\description{
Converts tags into a hash-tagged string
}
