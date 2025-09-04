safe_convergence <- function(m) {
  conv <- try(performance::check_convergence(m), silent = TRUE)
  if (is.null(conv)) {
    return(NA)
  }
  conv
}
