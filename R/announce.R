#' Announce a Blog Post Across Multiple Platforms
#'
#' The `announce_post` function automates the
#' task of announcing a blog post.
#' It extracts metadata from the post's YAML
#' front matter, generates relevant
#' messages, and posts announcements to
#' various platforms: Bluesky, LinkedIn,
#' Mastodon, and via newsletter.
#'
#' @param post A character vector of length
#'     one, representing the file path to
#'     the blog post. The function only supports
#'     a single file path and uses the
#'     first value if provided with more than
#'     one.
#'
#' @return The function does not explicitly
#'     return a value (`NULL`). It performs
#'     side effects by creating posts on
#'     external platforms and sending out a
#'     newsletter.
#'
#' @details
#' 1. Checks that the `post` argument is
#'    provided and raises errors or warnings
#'    as needed.
#' 2. Reads the YAML front matter of the blog
#'    post to extract metadata such as
#'    tags, slug, image, and additional fields
#'    used for announcements.
#' 3. Converts tags into a hashtag-based format.
#' 4. Constructs a shortened URL for the blog
#'    post based on its slug.
#' 5. Posts the announcement to:
#'    - **Bluesky**: Posts a truncated message
#'      along with an image.
#'    - **LinkedIn**: Posts the full summary
#'      along with an image.
#'    - **Mastodon**: Posts the SEO description,
#'      image, and alternative text.
#' 6. Sends the metadata to generate a
#'    newsletter notification.
#'
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom here here
#' @importFrom bskyr bs_post
#' @importFrom rtoot post_toot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' post <- "blog/post/2023-10-05-my-blog-post/index.md"
#' announce_post(post)
#' }
announce_post <- function(post) {
  # Check if arguments are provided
  if (is.null(post)) {
    cli::cli_abort(
      "Argument {.code post} does not have a value."
    )
  } else if (length(post) > 1) {
    cli::cli_alert_warning(
      "{.code post} should not have more than one 
      value. Processing only first one."
    )
    post <- post[1]
  }

  frontmatter <- rmarkdown::yaml_front_matter(post)

  # build URL
  url <- sprintf(
    "https://rladies.org/blog/%s/%s",
    basename(dirname(dirname(post))),
    frontmatter$slug
  )
  uri <- short_url(url)

  image <- here::here(dirname(post), frontmatter$image)

  body <- create_message(frontmatter, uri)

  resp <- bskyr::bs_post(
    text = body,
    images = image,
    images_alt = frontmatter$image_alt
  )
  cli::cli_alert_success("Posted to Bluesky {.url resp}")

  # Post to LinkedIn
  resp <- li_post_write(
    author = li_urn_me(),
    image_alt = frontmatter$image_alt,
    image = image,
    text = body
  )
  cli::cli_alert_success("Posted to LinkedIn {.url resp}")

  # Post to Mastodon
  resp <- rtoot::post_toot(
    status = body,
    media = image,
    alt_text = frontmatter$image_alt,
    visibility = "public",
    language = "US-en"
  )
  cli::cli_alert_success("Posted to Mastodon {.url resp}")
}
