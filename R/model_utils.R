safe_convergence <- function(m) {
  # Suppress warnings, messages, and any printed output from upstream
  conv <- NULL
  tmp <- utils::capture.output({
    conv <- try(
      suppressWarnings(
        suppressMessages(
          performance::check_convergence(m)
        )
      ),
      silent = TRUE
    )
  })

  if (is.null(conv)) {
    return(NA)
  }
  conv
}
