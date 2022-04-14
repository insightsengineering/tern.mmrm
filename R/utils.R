## adapted from purrr, but we do not need the output saved
capture_ouput <- function(code) {
  warnings <- character()
  wHandler <- function(w) {
    warnings <<- c(warnings, w$message)
    invokeRestart("muffleWarning")
  }
  messages <- character()
  mHandler <- function(m) {
    messages <<- c(messages, m$message)
    invokeRestart("muffleMessage")
  }
  invisible(utils::capture.output(result <- withCallingHandlers(code, warning = wHandler, message = mHandler)))
  list(result = result, warnings = warnings, messages = messages)
}
