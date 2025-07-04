% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linkedin.R
\name{li_client}
\alias{li_client}
\title{Create an OAuth Client for LinkedIn API}
\usage{
li_client()
}
\value{
An OAuth client object of class
\code{httr2_request} that can be used to
authenticate and make requests to the
LinkedIn API.
}
\description{
The \code{li_client} function initializes an
OAuth client to authenticate and
interact with the LinkedIn API. It uses
environment variables to retrieve
the LinkedIn application credentials
(client ID and client secret).
}
\details{
The function uses the \code{httr2::oauth_client}
function to create an OAuth client
for LinkedIn. The following environment
variables must be set:
\describe{
\item{\code{LI_CLIENT_ID}}{The LinkedIn
application client ID.}
\item{\code{LI_CLIENT_SECRET}}{The LinkedIn
application client secret.}
}
}
\examples{
\dontrun{
# Create an OAuth client for LinkedIn
client <- li_client()

client
}

}
