# Define generics

#' h_tab
#'
#' @param m1 A model to describe
#' @param m0 a nested, less complicated model
#' @param model_name The display name of m1
#' @export
#'
#' @examples
#' m0  <- lm(mpg ~ hp, data = mtcars)
#' m1 <- lm(mpg ~ hp + wt, data = mtcars)
#' h_tab(m1, m0, model_name = "Horsepower and Weight")

methods::setGeneric("h_table_part",
                    function(m1,
                             m0 = NULL,
                             model_name,
                             ci_method = NULL,
                             transf = NULL,
                             transf_name = NULL)
                      standardGeneric("h_table_part"),
                    signature = "m1")

#' tab
#'
#' Assemble model info into data.frame

#' @param model model to tabulate
#' @param ci_method passed to parameters
#' @param transf function to transform coefficients (not SE)
#' @param transf_name name for transformed coefficient
#' @param replace named vector, names replaced with contents
#' @export

tab <- function(model,
                ci_method = NULL,
                transf = NULL,
                transf_name = NULL,
                replace = NULL) {

  tabby <- tab_model(model,
                     ci_method = ci_method,
                     transf = transf,
                     transf_name = transf_name)

  if (length(replace) > 0 & !identical(replace, FALSE)) {
    for (i in seq_along(replace)) {
      tabby$Term <-
        gsub(names(replace)[i], replace[i], tabby$Term)
    }
  }

  tabby

}

methods::setGeneric("tab_model",
                    function(model, ci_method = NULL,transf = NULL,
                             transf_name = NULL)
                      standardGeneric("tab_model"),
                    signature = "model")



tab_model.lm <- function(model,
                         ci_method = NULL,
                         transf = NULL,
                         transf_name = NULL) {
  tabby <-
    data.table::data.table(parameters::parameters(model, ci_method = ci_method))

  if (is.null(transf))
    transf <- function(x)
      x
  if (is.null(transf_name))
    transf_name <- "b"

  process <- function(x)
    papaja::print_num(transf(x))

  tabby$"est95" <-
    with(
      tabby,
      glue::glue(
        "{process(Coefficient)} [{process(CI_low)}, {process(CI_high)}]"
      )
    ) |>
    as.character()

  tabby <-
    tabby[, .(
      Term = Parameter,
      est95,
      b = papaja::print_num(Coefficient),
      SE = papaja::print_num(SE),
      t = papaja::print_num(t),
      p = papaja::print_p(p)
    )]

   names(tabby)[names(tabby) == "est95"] <- glue::glue("{transf_name} [95\\% CI]")

  if (identical(transf, function(x)
    x)) {
    tabby$b <- NULL
  }

  tabby

}

tab_model.glm <- function(model, ci_method = NULL, transf = NULL,
                             transf_name = NULL){

  family <- insight::get_family(model)
  type <- NA
  if(family$family == "binomial" & family$link == "logit") type <- "OR"
  if(family$family == "poisson" & family$link == "log") type <- "IRR"

  tabby_exp <- data.table::data.table(parameters::parameters(model, ci_method = ci_method, exponentiate = TRUE))[, .(Parameter, CI_low, CI_high, Coefficient)]
  tabby_ln <-data.table::data.table(parameters::parameters(model, ci_method = "wald"))[,.(lnEffect = Coefficient, SE = SE, z, p)]
  tabby <- cbind(tabby_exp, tabby_ln)


  tabby$"est95" <-
    with(
      tabby,
      glue::glue(
        "{papaja::print_num(Coefficient)} [{papaja::print_num(CI_low)}, {papaja::print_num(CI_high)}]"
      )
    ) |>
    as.character()

  if(is.na(type)) stop("GLM family not yet supported")

  tabby <-
    tabby[, .(
      Term = Parameter,
      est95,
      lnEffect = papaja::print_num(lnEffect),
      SE = papaja::print_num(SE),
      z = papaja::print_num(z),
      p = papaja::print_p(p)
    )]

  names(tabby)[names(tabby) == "est95"] <- glue::glue("{type} [95\\% CI]")
  names(tabby) <- gsub("Effect", type, names(tabby))

  tabby

}

tab_model.glmmTMB <- function(model, ci_method = NULL, transf = NULL,
                             transf_name = NULL){

  family <- insight::get_family(model)
  type <- NA
  if(family$family == "binomial" & family$link == "logit") type <- "OR"
  if(family$family == "poisson" & family$link == "log") type <- "IRR"

  tabby_exp <-
    data.table::data.table(
      parameters::parameters(
        model,
        ci_method = ci_method,
        exponentiate = TRUE,
        effects = "fixed"
      )
    )[, .(Parameter, CI_low, CI_high, Coefficient)]

  tabby_ln <-
    data.table::data.table(parameters::parameters(model, ci_method = "wald", effects = "fixed"))[, .(lnEffect = Coefficient, SE = SE, z, p)]

    if("Component" %in% names(tabby_exp)){
    tabby_exp <- tabby_exp[Component != "zero_inflated"]
    tabby_ln <- tabby_ln[Component != "zero_inflated"]
    }

  tabby <- cbind(tabby_exp, tabby_ln)

  tabby$"est95" <-
    with(
      tabby,
      glue::glue(
        "{papaja::print_num(Coefficient)} [{papaja::print_num(CI_low)}, {papaja::print_num(CI_high)}]"
      )
    ) |>
    as.character()

  if(is.na(type)) stop("Unknown model family and type combination")

  tabby <-
    tabby[, .(
      Term = Parameter,
      est95,
      lnEffect = papaja::print_num(lnEffect),
      SE = papaja::print_num(SE),
      z = papaja::print_num(z),
      p = papaja::print_p(p)
    )]

  names(tabby)[names(tabby) == "est95"] <- glue::glue("{type} [95\\% CI]")
  names(tabby) <- gsub("Effect", type, names(tabby))
  tabby

}


#' h_tab
#'
#' Present results including a model comparison
#'
#' @param m1 A model to describe
#' @param m0 a nested, less complicated model
#' @param model_name The display name of m1
#' @import data.table
#'
#' @examples
#' m0  <- lm(mpg ~ hp, data = mtcars)
#' m1 <- lm(mpg ~ hp + wt, data = mtcars)
#' h_tab(m1, m0, model_name = "Horsepower and Weight")


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
  names(model_info) <- c(names(tabby), "R2", "$\\triangle$R2")

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
           ci_method = "NULL",
           transf = NULL,
           transf_name = NULL) {
    tabby <- tab_model(m1, ci_method = ci_method,
                       transf = transf,
                       transf_name = transf_name)

    model_name <- glue::glue("{model_name}") |>
      as.character()

    model_start <- c(model_name, rep("", ncol(tabby) - 1))

    if (!is.null(m0)) {
      comp  <- anova(m0, m1, test = "LRT")
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

h_table_part.glmmTMB <-
  function(m1,
           m0 = NULL,
           model_name = "untitled model",
           ci_method = "NULL",
           transf = NULL,
           transf_name = NULL) {


    tabby <- tab_model(m1, ci_method = ci_method,
                       transf = transf,
                       transf_name = transf_name)

    model_name <- glue::glue("{model_name}") |>
      as.character()

    model_start <- c(model_name, rep("", ncol(tabby) - 1))

    if (!is.null(m0)) {
      comp  <- anova(m0, m1, test = "LRT")

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

#' h_tab
#' @param ... a list of models, otherwise named models
#' @param ci_method what method should be used to derive confidence intervals?
#' @param transf function to transform coef and 95% CI
#' @param transf_name what should the transformed column be called?
#' @export

h_tab <- function(..., ci_method = NULL, transf = NULL,
                             transf_name = NULL){

models <- list(...)

if(length(models) == 1){
  models <- models[[1]]
}

model_names <- names(models)

 if(is.null(model_names)){
    model_names <- paste("Model", seq_along(models))
 }
 if(length(model_names) != length(models)){
   stop("Model names must be same length as models")
 }

out <- lapply(seq_along(models), function(m){
  if(m != 1) {
    m0 <- models[[m - 1]]
  }else{
    m0 <- NULL
  }
  h_table_part(m1 = models[[m]], m0 = m0,
               model_name = model_names[m], ci_method = ci_method, transf = transf,
                             transf_name = transf_name)
}) |> data.table::rbindlist()

is_coef <- out$is_coef
out$is_coef <- NULL
attr(out, "indent") <- list(is_coef)
class(out) <- c("h_tab", class(out))
out
}

# Define methods

methods::setMethod("tab_model", "lm", tab_model.lm)
methods::setMethod("tab_model", "glm", tab_model.glm)
methods::setMethod("tab_model", "glmmTMB", tab_model.glmmTMB)
methods::setMethod("h_table_part", "lm", h_table_part.lm)
methods::setMethod("h_table_part", "glm", h_table_part.glm)
methods::setMethod("h_table_part", "glmmTMB", h_table_part.glmmTMB)
