% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linkedin.R
\name{li_req}
\alias{li_req}
\title{Create a Base LinkedIn API Request}
\usage{
li_req(endpoint_version = "rest", ...)
}
\arguments{
\item{endpoint_version}{A string specifying
the LinkedIn API endpoint version. Defaults
to \code{"rest"}. Other possible values include
\code{"v2"} for specific API versions.}

\item{...}{Additional arguments passed to the
\code{li_req_auth()} function for authentication
configuration.}
}
\value{
An HTTP request object with default
headers and authentication configured,
which can be used with other \code{httr2}
methods to perform API queries.
}
\description{
This function creates a pre-configured HTTP
request object for interacting with the
LinkedIn API. It sets the API endpoint
version, essential headers, and includes
authentication.
}
\details{
The function initializes a request targeting
the LinkedIn API's base URL
(\verb{https://api.linkedin.com}) and appends the
specified endpoint version to the URL path.
It sets headers for API versioning, protocol
compatibility, and \code{Content-Type}.
Authentication is handled through
\code{li_req_auth()}.
}
\examples{
\dontrun{
# Create a request with default settings
req <- li_req()

# Create a request object
req <- li_req(
    endpoint_version = "v2",
    token = Sys.getenv("LI_TOKEN")
)
}

}
