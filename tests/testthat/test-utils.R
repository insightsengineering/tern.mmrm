testthat::test_that("capture_output suppresses output", {
  output <- testthat::capture_output(capture_output(cat("hello")))
  expect_equal(output, "")
})


testthat::test_that("capture_output shows result, warnings and messages", {
  f <- function(x) {
    message("message 1")
    warning("This is a warning")
    warning("This is another warning")
    message("message 2")
    return(x)
  }

  result <- capture_output(f("foo"))
  expect_equal(result$result, "foo")
  expect_equal(result$warnings, c("This is a warning", "This is another warning"))
  expect_equal(result$messages, c("message 1\n", "message 2\n"))
})
