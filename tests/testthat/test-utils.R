testthat::test_that("short_url function works correctly", {
  # Mock req_perform function to return our mock_response
  mock_perform <- function(req) {
    httr2::response(
      status_code = 200,
      body = jsonlite::toJSON(list(
        shortURL = "https://go.rladies.org/shortened"
      ))
    )
  }

  # Replace req_perform with mock_perform for testing
  with_mocked_responses(
    httr2::req_perform = mock_perform,
    {
      # Test valid URI input
      test_that("short_url correctly shortens valid URL", {
        result <- short_url("https://www.example.com/very-long-url")
        expect_equal(result, "https://go.rladies.org/shortened")
      })

      # Test invalid / malformed URL input
      test_that("short_url handles invalid URL inputs gracefully", {
        expect_error(short_url("invalid-url"), "Invalid URL format")
      })
    }
  )
})
