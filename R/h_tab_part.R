# Define generics

methods::setGeneric("h_table_part",
                    function(m1,
                             m0 = NULL,
                             model_name,
                             ci_method = NULL,
                             transf = NULL,
                             transf_name = NULL)
                      standardGeneric("h_table_part"),
                    signature = "m1")

#' h_tab
#'
#' Present results including a model comparison
#'
#' @param m1 A model to describe
#' @param m0 a nested, less complicated model
#' @param model_name The display name of m1
#' @importFrom data.table data.table
#'
#' @examples
#' m0  <- lm(mpg ~ hp, data = mtcars)
#' m1 <- lm(mpg ~ hp + wt, data = mtcars)
#' h_tab(m0, m1)


h_table_part.lm <- function(m1, m0 = NULL, model_name = "untitled model", ci_method = "NULL", transf = NULL,
                             transf_name = NULL) {

  tabby <- tab_model(m1, ci_method = ci_method,
                       transf = transf,
                       transf_name = transf_name)

  # Model info
  R2 <- unlist(performance::r2(m1)[2]) * 100

  model_name <- glue::glue("{model_name}") |>
    as.character()

  model_start <- c(model_name, rep("", ncol(tabby) - 1))

  if (!is.null(m0)) {
    R2_simple <- unlist(performance::r2(m0)[2]) * 100
    R2_diff <- (R2 - R2_simple) |>
      papaja::print_num() |>
      paste0("%")

  } else{
    R2_diff <- " "
  }

  R2 <- papaja::print_num(R2) |>
    paste0("%")

  model_info <-
    data.frame(matrix(c(model_start, R2, R2_diff), nrow = 1))
  names(model_info) <- c(names(tabby), "R2", "$\\triangle R^2$")

  tab_out <-
    data.table::rbindlist(list(model_info, tabby), fill = TRUE)
  tab_out[is.na(tab_out)] <- " "
  tab_out$is_coef <- TRUE
  tab_out$is_coef[1] <- FALSE
  tab_out
}

h_table_part.glm <-
  function(m1,
           m0 = NULL,
           model_name = "untitled model",
           ci_method = NULL,
           transf = NULL,
           transf_name = NULL) {
    tabby <- tab_model(m1, ci_method = ci_method,
                       transf = transf,
                       transf_name = transf_name)

    model_name <- glue::glue("{model_name}") |>
      as.character()

    model_start <- c(model_name, rep("", ncol(tabby) - 1))

    if (!is.null(m0)) {
      comp  <- stats::anova(m0, m1, test = "LRT")
      LRT <-
        with(
          comp[2, ],
          glue::glue(
            "$\\chi^2$({Df}) = {papaja::print_num(Deviance)}, $p$ = {papaja::print_p(`Pr(>Chi)`)}"
          )
        )

    } else{
      LRT <- " "
    }

    model_info <-
      data.frame(matrix(c(model_start, LRT), nrow = 1))
    names(model_info) <- c(names(tabby), "Likelihood Ratio Test")

    tab_out <-
      data.table::rbindlist(list(model_info, tabby), fill = TRUE)
    tab_out[is.na(tab_out)] <- " "
    tab_out$is_coef <- TRUE
    tab_out$is_coef[1] <- FALSE
    tab_out
  }

h_table_part.lmerMod <- function(m1,
                                 m0 = NULL,
                                 model_name = "untitled model",
                                 ci_method = NULL,
                                 transf = NULL,
                                 transf_name = NULL) {


  tabby <- tab_model(m1, ci_method = ci_method,
                     transf = transf,
                     transf_name = transf_name)

  model_name <- glue::glue("{model_name}") |>
    as.character()

  model_start <- c(model_name, rep("", ncol(tabby) - 1))

  if (!is.null(m0)) {
    comp  <- stats::anova(m0, m1, test = "LRT")

    LRT <-
      with(
        comp[2, ],
        glue::glue(
          "$\\chi^2$({`Df`}) = {papaja::print_num(Chisq)}, $p$ = {papaja::print_p(`Pr(>Chisq)`)}"
        )
      )

  } else{
    LRT <- " "
  }

  model_info <-
    data.frame(matrix(c(model_start, LRT), nrow = 1))
  names(model_info) <- c(names(tabby), "Likelihood Ratio Test")

  tab_out <-
    data.table::rbindlist(list(model_info, tabby), fill = TRUE)
  tab_out[is.na(tab_out)] <- " "
  tab_out$is_coef <- TRUE
  tab_out$is_coef[1] <- FALSE
  tab_out
}

h_table_part.glmmTMB <-
  function(m1,
           m0 = NULL,
           model_name = "untitled model",
           ci_method = NULL,
           transf = NULL,
           transf_name = NULL) {


    tabby <- tab_model(m1, ci_method = ci_method,
                       transf = transf,
                       transf_name = transf_name)

    model_name <- glue::glue("{model_name}") |>
      as.character()

    model_start <- c(model_name, rep("", ncol(tabby) - 1))

    if (!is.null(m0)) {
      comp  <- stats::anova(m0, m1, test = "LRT")

      LRT <-
        with(
          comp[2, ],
          glue::glue(
            "$\\chi^2$({`Chi Df`}) = {papaja::print_num(Chisq)}, $p$ = {papaja::print_p(`Pr(>Chisq)`)}"
          )
        )

    } else{
      LRT <- " "
    }

    model_info <-
      data.frame(matrix(c(model_start, LRT), nrow = 1))
    names(model_info) <- c(names(tabby), "Likelihood Ratio Test")

    tab_out <-
      data.table::rbindlist(list(model_info, tabby), fill = TRUE)
    tab_out[is.na(tab_out)] <- " "
    tab_out$is_coef <- TRUE
    tab_out$is_coef[1] <- FALSE
    tab_out
  }

h_table_part.glmerMod <- function(m1,
                                  m0 = NULL,
                                  model_name = "untitled model",
                                  ci_method  = NULL,
                                  transf      = NULL,
                                  transf_name = NULL) {

  tabby <- tab_model(m1,
                     ci_method   = ci_method,
                     transf      = transf,
                     transf_name = transf_name)

  model_name  <- as.character(glue::glue("{model_name}"))
  model_start <- c(model_name, rep("", ncol(tabby) - 1))

  ## likelihood-ratio test for nested comparison (if supplied)
  LRT <- " "
  if (!is.null(m0)) {
    cmp <- stats::anova(m0, m1, test = "LRT")
    LRT <- with(cmp[2, ],
                glue::glue("$\\chi^2$({Df}) = {papaja::print_num(Chisq)}, ",
                           "$p$ = {papaja::print_p(`Pr(>Chisq)`)}"))
  }

  model_info <- data.frame(matrix(c(model_start, LRT), nrow = 1))
  names(model_info) <- c(names(tabby), "Likelihood Ratio Test")

  out <- data.table::rbindlist(list(model_info, tabby), fill = TRUE)
  out[is.na(out)] <- " "
  out$is_coef <- TRUE; out$is_coef[1] <- FALSE
  out
}

# glmmTMB is s3, need to set is up as an old s4 class
if (!methods::isClass("glmmTMB"))
  methods::setOldClass("glmmTMB")

methods::setMethod("h_table_part", "lm", h_table_part.lm)
methods::setMethod("h_table_part", "glm", h_table_part.glm)
methods::setMethod("h_table_part", "glmmTMB", h_table_part.glmmTMB)
methods::setMethod("h_table_part", "lmerMod", h_table_part.lmerMod)
methods::setMethod("h_table_part", "glmerMod", h_table_part.glmerMod)

