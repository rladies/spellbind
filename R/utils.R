#' Shorten a URL using the Short.io API
#'
#' This function takes a given URL and
#' leverages the Short.io API to generate
#' a shortened version of the URL using
#' the specified custom domain.
#'
#' @param uri A character string. The
#'     original URL to be shortened.
#'
#' @return A character string. The
#'     shortened URL returned by the Short.io API.
#'
#' @description
#' `short_url` sends a POST request to the
#' Short.io API to create a new shortened
#' URL for the provided `uri`. The function
#' uses environment variable `SHORTIO`
#' for the API key and sets specific options
#' for the API call, such as not
#' allowing duplicate links and setting a
#' fixed domain for the shortened URL.
#'
#' @examples
#' \dontrun{
#'   # Example usage to shorten a URL
#'   Sys.setenv(SHORTIO = "your_api_key_here") # Set API key in environment
#'   short_url("https://www.example.com/very-long-url")
#' }
#'
#' @importFrom httr2 request req_method req_headers req_body_json req_perform
#' @importFrom httr2 resp_body_json
#' @export
short_url <- function(uri) {
  message("Getting Short.io")
  resp <- httr2::request("https://api.short.io/links") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = Sys.getenv("SHORTIO"),
      accept = "application/json",
      `content-type` = "application/json",
    ) |>
    httr2::req_body_json(
      data = list(
        skipQS = FALSE,
        archived = FALSE,
        allowDuplicates = FALSE,
        originalURL = uri,
        domain = "go.rladies.org"
      ),
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  resp$shortURL
}

#' Compute the Number of Characters in a String
#'
#' This function calculates the total number of
#' characters in a given string.
#'
#' @param x A character string. The input string
#' whose length is to be calculated.
#'
#' @return An integer. The total number of
#' characters in the input string.
#'
#' @description
#' `strlength` splits the input string into
#' individual characters and counts them
#' using `strsplit` and `length`. It is
#' equivalent to determining the string length.
#'
#' @examples
#' # Example usage:
#' strlength("hello")        # Returns: 5
#' strlength("R is great!")  # Returns: 11
#'
#' @export
strlength <- function(x) {
  strsplit(x, "") |>
    unlist() |>
    length()
}

#' Converts tags into a hash-tagged string
#'
#' @param tags A character vector of tags
#'     (e.g., c("R", "Health data")).
#' @return A single string with tags
#'     converted to a hash-tagged format
#'     (e.g., "#Rstats #HealthData").
#'     Specific transformation applied:
#'      - Converts "r" (case insensitive)
#'        to "rstats".
#'      - Removes spaces within tags.
#'      - Collapses all elements into one
#'        space-separated string.
tags2hash <- function(tags) {
  tags <- paste0("#", tags)
  tags <- sub("^#r$", "#rstats", tags, ignore.case = TRUE)
  tags <- sub(" |-", "", tags, ignore.case = TRUE)
  paste(tags, collapse = " ")
}

#' Create a Formatted Announcement Message
#'
#' The `create_message` function generates
#' a formatted message string using
#' information from the blog post's metadata,
#' including the title, custom text,
#' a randomly selected emoji, a URL, and
#' associated tags.
#'
#' @param text A character string providing
#'     the main content to include in the
#'     message body. The text should not exceed
#'     a reasonable length for platform
#'     constraints.
#'
#' @return A character string containing the
#'   formatted message, which includes:
#'   \describe{
#'     \item{Title}{The title of the blog post.}
#'     \item{Emoji}{A randomly generated emoji
#'     to add personality to the message.}
#'     \item{Text}{The input `text`, passed in
#'     its entirety.}
#'     \item{URL}{A link to the blog post.}
#'     \item{Tags}{Hashtags derived from the blog's metadata.}
#'   }
#'
#' @details
#' The function uses the `glue` package to
#' assemble a string with placeholders
#' for pre-defined variables, including the
#' blog post title, emoji, URI, and tags.
#' These variables should exist in the
#' global environment or accessible scope
#' when this function is called.
#'
#' @examples
#' \dontrun{
#' # Assuming variables are defined in
#' # the global environment:
#' frontmatter <- list(
#'   title = "Learning R Programming"
#' )
#' emoji <- "🔥"
#' uri <- "https://example.com/a-blog-post"
#' tags <- "#RStats #DataScience"
#'
#' # Create a message
#' create_message("Check out this insightful blog post!")
#' }
#'
#' @importFrom glue glue
#'
#' @export
create_message <- function(
  frontmatter,
  uri
) {
  emoji <- random_emoji()
  tags <- tags2hash(frontmatter$tags)

  glue::glue(
    "'{frontmatter$title}'
  
  {emoji} {frontmatter$description} 
  
  👀 {uri} 
  
  {tags}"
  )
}

#' Generate a Random Emoji
#'
#' The `random_emoji` function selects and
#' returns a random emoji from a predefined
#' list of emojis. This function is often
#' used to add personality or fun to
#' messages and announcements.
#'
#' @return A single character string
#'     containing a randomly selected emoji.
#'
#' @details
#' The function draws from a fixed list
#' of commonly used emojis. Each time the
#' function is called, it selects one emoji
#' at random and returns it. No input
#' parameters are required.
#'
#' @examples
#' # Generate a random emoji
#' random_emoji()
#'
#' # Assign a random emoji to a variable
#' my_emoji <- random_emoji()
#' my_emoji
#'
#' @export
random_emoji <- function() {
  emojis <- c(
    "🦄",
    "🦜",
    "🦣",
    "🦥",
    "🦦",
    "🦧",
    "🦨",
    "🦩",
    "🦪",
    "🦫",
    "🦬",
    "🦭",
    "🦮",
    "🦯",
    "🦰",
    "🦱",
    "🦲",
    "🦳",
    "🦴",
    "🦵",
    "🦶",
    "🦷",
    "🦸",
    "🦹",
    "🦺",
    "🦻",
    "🦼",
    "🦽",
    "🦾",
    "🦿",
    "🧀",
    "🧁",
    "🧂",
    "🧃",
    "🧄",
    "🧅",
    "🧆",
    "🧇",
    "🧈",
    "🧉",
    "🧊",
    "🧋",
    "🧌",
    "🧍",
    "🧎",
    "🧏",
    "🧐",
    "🧑",
    "🧒",
    "🧓",
    "🧔",
    "🧕",
    "🧖",
    "🧗",
    "🧘",
    "🧙",
    "🧚",
    "🧛",
    "🧜",
    "🧝",
    "🧞",
    "🧟",
    "🧠",
    "🧡",
    "🧢",
    "🧣",
    "🧤",
    "🧥",
    "🧦",
    "🧧",
    "🧨",
    "🧩",
    "🧪",
    "🧫",
    "🧬",
    "🧭",
    "🧮",
    "🧯",
    "🧰",
    "🧱",
    "🧲",
    "🧳",
    "🧴",
    "🧵",
    "🧶",
    "🧷",
    "🧸",
    "🧹",
    "🧺",
    "🧻",
    "🧼",
    "🧽",
    "🧾",
    "🧿"
  )

  sample(emojis, 1)
}
