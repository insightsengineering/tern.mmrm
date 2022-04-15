## adapted from purrr, but we do not need the output saved
capture_output <- function(code) {
  warnings <- character()
  w_handler <- function(w) {
    warnings <<- c(warnings, w$message)
    invokeRestart("muffleWarning")
  }
  messages <- character()
  m_handler <- function(m) {
    messages <<- c(messages, m$message)
    invokeRestart("muffleMessage")
  }
  invisible(utils::capture.output(result <- withCallingHandlers(code, warning = w_handler, message = m_handler)))
  list(result = result, warnings = warnings, messages = messages)
}
