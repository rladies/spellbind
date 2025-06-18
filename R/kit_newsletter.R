#' Send a Newsletter Using ConvertKit API
#'
#' The `send_newsletter` function sends a
#' newsletter created using metadata
#' extracted from a blog post's front matter.
#' It communicates with the
#' ConvertKit API to create and schedule an
#' email broadcast.
#'
#' @param frontmatter A named list containing
#'   metadata extracted from the blog
#'   post YAML. The list should include the
#'   following fields:
#'   \describe{
#'     \item{`seo`}{A string containing the SEO
#'     description for the blog post.}
#'     \item{`image`}{A string with the relative
#'     path to the blog post's thumbnail image.}
#'     \item{`title`}{A string containing the
#'     blog post's title.}
#'   }
#' @param url Character, url of the post.
#'
#' @return This function does not explicitly
#'    return a value (`NULL`). It performs
#'    a side effect by sending a newsletter
#'    through the ConvertKit API.
#'
#' @details
#' The function generates the HTML content for
#' the newsletter using the `glue`
#' package (placeholders for custom templates
#' need to be defined). It then
#' constructs an HTTP POST request to the
#' ConvertKit API, passing various fields
#' such as the email `subject`, `thumbnail_url`,
#' and `content`. The API key for
#' authentication is fetched from the
#' environment variable `KIT_SECRET`.
#'
#' @examples
#' \dontrun{
#' # Example frontmatter
#' frontmatter <- list(
#'   seo = "This is an amazing blog post about R programming!",
#'   image = "images/blog-thumbnail.jpg",
#'   title = "Learning R Programming"
#' )
#'
#' # Sending a newsletter
#' send_newsletter(frontmatter)
#' }
#'
#' @importFrom glue glue
#' @importFrom httr2 request req_body_json
#'   req_perform
#'
#' @export
send_newsletter <- function(frontmatter, url) {
  newsletter <- glue::glue(
    "add html template here"
  )

  httr2::request("https://api.convertkit.com/v3/broadcasts") |>
    httr2::req_body_json(
      list(
        public = TRUE,
        api_secret = Sys.getenv("KIT_SECRET"),
        description = frontmatter$seo,
        thumbnail_url = file.path(
          url,
          frontmatter$image
        ),
        subject = frontmatter$title,
        content = newsletter,
        send_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
      )
    ) |>
    httr2::req_perform()
}
