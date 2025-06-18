#' LinkedIn Chars to escape
#' @noRd
escape_linkedin_chars <- function(x) {
  chars <- c(
    "\\|",
    "\\{",
    "\\}",
    "\\@",
    "\\[",
    "\\]",
    "\\(",
    "\\)",
    "\\<",
    "\\>",
    "\\#",
    "\\\\",
    "\\*",
    "\\_",
    "\\~"
  )
  p <- stats::setNames(paste0("\\", chars), chars)
  stringr::str_replace_all(x, p)
}

#' Get the LinkedIn Version Date in YYYYMM Format
#'
#' The `li_get_version` function calculates the
#' most recent past month's LinkedIn
#' version date and formats it as a string in
#' `YYYYMM` format. This date is used
#' to reference LinkedIn-specific API versions
#' or other functionalities tied to
#' monthly updates.
#'
#' @return A character string representing the
#'    version date in `YYYYMM` format,
#'    where `YYYY` is the 4-digit year, and `MM`
#'    is the 2-digit month.
#'
#' @details
#' The function calculates the version date by
#' taking the current date and
#' rolling back to the previous month. The
#' result is then formatted as a string
#' in `YYYYMM` format using the `lubridate` and
#' `stringr` packages.
#'
#' @examples
#' \dontrun{
#' li_get_version()
#'
#' # Store the version date in a variable
#' li_get_version()
#' }
#'
#' @importFrom lubridate rollback today year
#'   month
#' @importFrom stringr str_pad
#'
#' @export
li_get_version <- function() {
  li_version_date <- lubridate::rollback(lubridate::today())

  paste0(
    lubridate::year(li_version_date),
    stringr::str_pad(lubridate::month(li_version_date), 2, pad = "0")
  )
}

#' Create an OAuth Client for LinkedIn API
#'
#' The `li_client` function initializes an
#' OAuth client to authenticate and
#' interact with the LinkedIn API. It uses
#' environment variables to retrieve
#' the LinkedIn application credentials
#' (client ID and client secret).
#'
#' @return An OAuth client object of class
#'     `httr2_request` that can be used to
#'     authenticate and make requests to the
#'     LinkedIn API.
#'
#' @details
#' The function uses the `httr2::oauth_client`
#' function to create an OAuth client
#' for LinkedIn. The following environment
#' variables must be set:
#'   \describe{
#'     \item{`LI_CLIENT_ID`}{The LinkedIn
#'     application client ID.}
#'     \item{`LI_CLIENT_SECRET`}{The LinkedIn
#'     application client secret.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Create an OAuth client for LinkedIn
#' client <- li_client()
#'
#' client
#' }
#'
#' @importFrom httr2 oauth_client
li_client <- function() {
  httr2::oauth_client(
    name = "rladies_linkedIn",
    token_url = "https://www.linkedin.com/oauth/v2/accessToken",
    id = Sys.getenv("LI_CLIENT_ID"),
    secret = Sys.getenv("LI_CLIENT_SECRET")
  )
}


#' Perform LinkedIn OAuth Authentication
#'
#' The `li_oauth` function facilitates OAuth
#' 2.0 authentication for interacting with
#' the LinkedIn API. It uses the authorization
#' code flow to generate an OAuth token.
#'
#' @return An OAuth token object (of class
#' `httr2_token`) that can be used to
#' authenticate subsequent API requests to
#' LinkedIn.
#'
#' @details
#' The function performs the following steps:
#'   1. Calls `li_client()` to initialize the
#'      OAuth client using the LinkedIn
#'      client ID and secret (stored as
#'      environment variables: `LI_CLIENT_ID`
#'      and `LI_CLIENT_SECRET`).
#'   2. Constructs the authorization URL with
#'      the required state, scope, and client
#'      details.
#'   3. Executes the authorization code flow
#'      using `httr2::oauth_flow_auth_code`,
#'      which:
#'       - Prompts the user to allow access.
#'       - Supports redirection to
#'       `http://localhost:1444/` to complete
#'       the authorization process.
#'       - Specifies required LinkedIn
#'       permissions (scope): email, openid,
#'       profile, and w_member_social.
#'
#' @examples
#' \dontrun{
#' # Perform LinkedIn OAuth authentication and save the token
#' token <- li_oauth()
#'
#' # Use the token for authenticated API requests
#' token
#' }
#'
#' @importFrom httr2 oauth_flow_auth_code
#'     oauth_flow_auth_code_url
#'
#' @export
li_oauth <- function() {
  auth_url <- "https://www.linkedin.com/oauth/v2/authorization"

  auth_url <- httr2::oauth_flow_auth_code_url(
    client = li_client(),
    auth_url = auth_url,
    state = "whoruntheworldgirls"
  )

  httr2::oauth_flow_auth_code(
    client = li_client(),
    auth_url = auth_url,
    redirect_uri = "http://localhost:1444/",
    scope = paste("email", "openid", "profile", "w_member_social"),
    pkce = FALSE
  )
}


#' Create authorization for LinkedIn
#'
#' This uses the bearer token auth, which would
#' not work for organisation posting.
#'
#' @param req httr2 request
#' @param token Access token
#'
#' @noRd
li_req_auth <- function(req, token = Sys.getenv("LI_TOKEN")) {
  req |>
    httr2::req_auth_bearer_token(token)
}


#' Create a Base LinkedIn API Request
#'
#' This function creates a pre-configured HTTP
#' request object for interacting with the
#' LinkedIn API. It sets the API endpoint
#' version, essential headers, and includes
#' authentication.
#'
#' @param endpoint_version A string specifying
#'   the LinkedIn API endpoint version. Defaults
#'   to `"rest"`. Other possible values include
#'   `"v2"` for specific API versions.
#' @param ... Additional arguments passed to the
#'   `li_req_auth()` function for authentication
#'   configuration.
#'
#' @return An HTTP request object with default
#'   headers and authentication configured,
#'   which can be used with other `httr2`
#'   methods to perform API queries.
#'
#' @details
#' The function initializes a request targeting
#' the LinkedIn API's base URL
#' (`https://api.linkedin.com`) and appends the
#' specified endpoint version to the URL path.
#' It sets headers for API versioning, protocol
#' compatibility, and `Content-Type`.
#' Authentication is handled through
#' `li_req_auth()`.
#'
#' @examples
#' \dontrun{
#' # Create a request with default settings
#' req <- li_req()
#'
#' # Create a request object
#' req <- li_req(
#'     endpoint_version = "v2",
#'     token = Sys.getenv("LI_TOKEN")
#' )
#' }
#'
#' @importFrom httr2 request req_url_path_append
#'    req_headers
#' @export
li_req <- function(endpoint_version = "rest", ...) {
  httr2::request("https://api.linkedin.com") |>
    httr2::req_url_path_append(endpoint_version) |>
    httr2::req_headers(
      "LinkedIn-Version" = li_get_version(),
      "X-Restli-Protocol-Version" = "2.0.0",
      "Content-Type" = "application/json"
    ) |>
    li_req_auth(...)
}

#' Retrieve the LinkedIn URN of the User
#'
#' This function fetches the LinkedIn Uniform
#' Resource Name (URN) for the currently
#' authenticated user using the LinkedIn API.
#' Authentication is performed using a
#' bearer token stored in the `LI_TOKEN`
#' environment variable.
#'
#' @return A string representing the user's
#' LinkedIn URN in the format `"urn:li:person:{id}"`.
#'
#' @details
#' The function sends an API request to the
#' LinkedIn `userinfo` endpoint with the
#' appropriate authorization token and extracts
#' the user's unique identifier (sub).
#' This identifier is then formatted as a
#' LinkedIn URN.
#'
#' @examples
#' \dontrun{
#' li_urn_me()
#' }
#' @importFrom httr2 req_url_path_append
#' @export
li_urn_me <- function() {
  id <- li_req("v2") |>
    httr2::req_url_path_append("userinfo") |>
    httr2::req_auth_bearer_token(
      Sys.getenv("LI_TOKEN")
    ) |>
    httr2::req_url_query(projection = "(sub)") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    unlist()
  paste0("urn:li:person:", id)
}

#' Create and Publish a LinkedIn Post
#'
#' This function publishes a new post to LinkedIn
#' using the specified author, text content,
#' optional image, and associated image alt text.
#' It supports adding media attachments and
#' retries requests for transient errors.
#'
#' @param author A string representing the unique
#'   LinkedIn author URN (Uniform Resource Name).
#' @param text A string representing the textual
#'   content of the LinkedIn post. The text is
#'   escaped to ensure LinkedIn compatibility.
#' @param image (Optional) A string representing
#'   the file path to the image that will be
#'   attached to the post. Defaults to `NULL`
#'   if no image is included.
#' @param image_alt (Optional) A string
#'   representing the alternate text for the
#'   image (e.g., for accessibility). Defaults
#'   to an empty string `""`.
#'
#' @return Invisibly returns the LinkedIn post
#'   ID as a string. Displays a message with the
#'   LinkedIn URL for the published post.
#'
#' @details
#' If an image is provided, the function
#' automatically uploads the image to LinkedIn
#' using the
#' `li_media_upload()` helper function (not
#' defined here). The post is configured for public
#' visibility and main feed distribution. Error
#' retries are applied for certain HTTP status
#' codes.
#'
#' @examples
#' \dontrun{
#' # Publish a simple text post:
#' li_post_write(
#'   author = "urn:li:person:exampleAuthor",
#'   text = "This is a simple LinkedIn post!"
#' )
#'
#' # Publish a post with an image:
#' li_post_write(
#'   author = "urn:li:person:exampleAuthor",
#'   text = "Check out this image!",
#'   image = "path/to/image.jpg",
#'   image_alt = "A description of the image for accessibility"
#' )
#' }
#'
#' @importFrom httr2 req_url_path_append
#' @export
li_post_write <- function(author, text, image = NULL, image_alt = "") {
  text <- escape_linkedin_chars(text)

  body <- list(
    author = author,
    lifecycleState = "PUBLISHED",
    commentary = text,
    visibility = "PUBLIC",
    distribution = list(
      `feedDistribution` = "MAIN_FEED",
      `targetEntities` = list(),
      `thirdPartyDistributionChannels` = list()
    ),
    isReshareDisabledByAuthor = FALSE
  )

  if (!is.null(image)) {
    body <- c(
      body,
      list(
        content = list(
          media = list(
            id = li_media_upload(author, image),
            title = image_alt
          )
        )
      )
    )
  }

  resp <- li_req() |>
    httr2::req_url_path_append("posts") |>
    httr2::req_body_json(
      body,
      auto_unbox = TRUE
    ) |>
    httr2::req_retry(
      is_transient = \(x) httr2::resp_status(x) %in% c(401, 403, 425, 429),
      max_tries = 10,
      backoff = ~3
    ) |>
    httr2::req_perform() |>
    httr2::resp_header("x-restli-id")

  message(file.path(
    "https://www.linkedin.com/feed/update/",
    resp
  ))

  invisible(resp)
}

#' Upload Media to LinkedIn
#'
#' The `li_media_upload` function uploads a media
#' file (e.g., an image) to LinkedIn
#' for use in posts or other content. It first
#' initializes the upload process with
#' LinkedIn’s API, then uploads the media file to
#' the provided upload URL.
#'
#' @param author A character string representing
#'   the LinkedIn profile or organization
#'   ID in the format `urn:li:person:XXXX` or
#'   `urn:li:organization:XXXX`. This identifies
#'   the owner of the media being uploaded.
#' @param media A character string representing
#'   the file path to the media (e.g., an
#'   image file) to be uploaded.
#'
#' @return A character string representing the
#'   LinkedIn image URL that can be used
#'   to reference the uploaded media in subsequent
#'   API calls.
#'
#' @details
#' The function performs the following steps:
#'   1. Initializes the media upload by sending a
#'      request to the LinkedIn API.
#'   2. Extracts the `uploadUrl` from the
#'      response, which specifies where to upload
#'      the media file.
#'   3. Uploads the media file to the `uploadUrl`.
#'   4. Returns the image URL generated by
#'      LinkedIn for referencing the uploaded file.
#'
#' The `li_req()` function, assumed to create an
#' authenticated LinkedIn API request,
#' must be available in the calling environment.
#'
#' @examples
#' \dontrun{
#' # Assign LinkedIn author and media to upload
#' author <- "urn:li:person:123456789"
#' media <- "path/to/image.png"
#'
#' # Upload the media file
#' image_url <- li_media_upload(author, media)
#'
#' # Use the returned image URL in a post
#' image_url
#' }
#'
#' @importFrom httr2 req_url_path_append
#'   req_url_query req_body_json req_body_file
#'   req_perform resp_body_json
#' @export
li_media_upload <- function(author, media) {
  r <- li_req() |>
    httr2::req_url_path_append("images") |>
    httr2::req_url_query(action = "initializeUpload") |>
    httr2::req_body_json(
      list(
        initializeUploadRequest = list(
          owner = author
        )
      ),
      auto_unbox = TRUE
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  httr2::request(r$value$uploadUrl) |>
    httr2::req_body_file(media) |>
    httr2::req_perform()

  r$value$image
}
