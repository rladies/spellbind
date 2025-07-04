% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linkedin.R
\name{li_media_upload}
\alias{li_media_upload}
\title{Upload Media to LinkedIn}
\usage{
li_media_upload(author, media)
}
\arguments{
\item{author}{A character string representing
the LinkedIn profile or organization
ID in the format \code{urn:li:person:XXXX} or
\code{urn:li:organization:XXXX}. This identifies
the owner of the media being uploaded.}

\item{media}{A character string representing
the file path to the media (e.g., an
image file) to be uploaded.}
}
\value{
A character string representing the
LinkedIn image URL that can be used
to reference the uploaded media in subsequent
API calls.
}
\description{
The \code{li_media_upload} function uploads a media
file (e.g., an image) to LinkedIn
for use in posts or other content. It first
initializes the upload process with
LinkedIn’s API, then uploads the media file to
the provided upload URL.
}
\details{
The function performs the following steps:
\enumerate{
\item Initializes the media upload by sending a
request to the LinkedIn API.
\item Extracts the \code{uploadUrl} from the
response, which specifies where to upload
the media file.
\item Uploads the media file to the \code{uploadUrl}.
\item Returns the image URL generated by
LinkedIn for referencing the uploaded file.
}

The \code{li_req()} function, assumed to create an
authenticated LinkedIn API request,
must be available in the calling environment.
}
\examples{
\dontrun{
# Assign LinkedIn author and media to upload
author <- "urn:li:person:123456789"
media <- "path/to/image.png"

# Upload the media file
image_url <- li_media_upload(author, media)

# Use the returned image URL in a post
image_url
}

}
