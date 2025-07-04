% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{strlength}
\alias{strlength}
\title{Compute the Number of Characters in a String}
\usage{
strlength(x)
}
\arguments{
\item{x}{A character string. The input string
whose length is to be calculated.}
}
\value{
An integer. The total number of
characters in the input string.
}
\description{
\code{strlength} splits the input string into
individual characters and counts them
using \code{strsplit} and \code{length}. It is
equivalent to determining the string length.
}
\details{
This function calculates the total number of
characters in a given string.
}
\examples{
# Example usage:
strlength("hello")        # Returns: 5
strlength("R is great!")  # Returns: 11

}
