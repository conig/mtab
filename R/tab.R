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


#' tab_model.lm
#'
#' tab_model method for linear models
#' @param model lm model
#' @param ci_method confidence interval method send to parameters::parameters
#' @param transf optional transformation function
#' @param transf_name optional transformation name

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
    tabby[, list(
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

#' tab_model.lmerMod
#'
#' tab_model method for lmerMod models
#' @param model lmerMod model
#' @param ci_method confidence interval method send to parameters::parameters
#' @param transf optional transformation function
#' @param transf_name optional transformation name

tab_model.lmerMod <- function(model,
                         ci_method = NULL,
                         transf = NULL,
                         transf_name = NULL) {
  tabby <-
    data.table::data.table(parameters::parameters(model, ci_method = ci_method))[Effects == "fixed"]

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
    tabby[, list(
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

#' tab_model.glm
#'
#' tab_model method for general linear models
#' @param model glm model
#' @param ci_method confidence interval method send to parameters::parameters
#' @param transf optional transformation function
#' @param transf_name optional transformation name

tab_model.glm <- function(model, ci_method = NULL, transf = NULL,
                             transf_name = NULL){

  family <- insight::get_family(model)
  type <- NA
  if(family$family == "binomial" & family$link == "logit") type <- "OR"
  if(family$family == "poisson" & family$link == "log") type <- "IRR"

  tabby_exp <- data.table::data.table(parameters::parameters(model, ci_method = ci_method, exponentiate = TRUE))[, list(Parameter, CI_low, CI_high, Coefficient)]
  tabby_ln <-data.table::data.table(parameters::parameters(model, ci_method = "wald"))[,list(lnEffect = Coefficient, SE = SE, z, p)]
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
    tabby[, list(
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

#' tab_model.glmmTM'
#' tab_model method for glmmTMB models
#' @param model glmmTMB model
#' @param ci_method confidence interval method send to parameters::parameters
#' @param transf optional transformation function
#' @param transf_name optional transformation name
#' @importFrom glmmTMB glmmTMB

tab_model.glmmTMB <- function(model,
  ci_method = NULL,
  transf = NULL,
  transf_name = NULL){

  family <- insight::get_family(model)
  type <- NA

  if(family$family == "binomial" & family$link == "logit") type <- "OR"
  if(family$family == "poisson" & family$link == "log") type <- "IRR"
  if(family$family == "gaussian" & family$link == "identity") type <- "b"

  if(is.na(type)) stop("Unknown model family and type combination")

  # Extract parameters
  params <-
    data.table::data.table(parameters::parameters(model, ci_method = ci_method, effects = "fixed"))

  params_exp <- data.table::data.table(parameters::parameters(model, ci_method = ci_method, effects = "fixed", exponentiate = TRUE))

  # Remove zero-inflation if present
   if("Component" %in% names(params)){
    params <- params[Component != "zero_inflated"]
    params_exp <- params_exp[Component != "zero_inflated"]
   }
  # Get relevant columns

  if(type != "b") {
  tabby_ln <-
      params[, list(lnEffect = Coefficient, SE = SE, z, p)]
  tabby_exp <-
     params_exp[, list(Parameter, CI_low, CI_high, Coefficient)]
  tabby <- cbind(tabby_exp, tabby_ln)
  }else{
  tabby <-
      params[, list(Parameter, Coefficient, CI_low, CI_high, SE, z, p)]
  }

  # Bind together exp and raw
  tabby$"est95" <-
    with(
      tabby,
      glue::glue(
        "{papaja::print_num(Coefficient)} [{papaja::print_num(CI_low)}, {papaja::print_num(CI_high)}]"
      )
    ) |>
    as.character()

  if(type != "b"){

    tabby <-
      tabby[, list(
        Term = Parameter,
        est95,
        lnEffect = papaja::print_num(lnEffect),
        SE = papaja::print_num(SE),
        z = papaja::print_num(z),
        p = papaja::print_p(p)
      )]

  } else{
    tabby <-
      tabby[, list(
        Term = Parameter,
        est95,
        SE = papaja::print_num(SE),
        z = papaja::print_num(z),
        p = papaja::print_p(p)
      )]
  }

  names(tabby)[names(tabby) == "est95"] <- glue::glue("{type} [95\\% CI]")
  names(tabby) <- gsub("Effect", type, names(tabby))
  tabby

}


#' tab_model.glmerMod
#'
#' Method for lme4::glmer models (generalised linear mixed models)
#' @param model  glmerMod object
#' @inheritParams tab_model.lm
#' @importFrom lme4 glmer
tab_model.glmerMod <- function(model,
                               ci_method  = NULL,
                               transf     = NULL,
                               transf_name = NULL) {

  fam  <- insight::get_family(model)
  type <- NA_character_

  if (fam$family == "binomial" && fam$link == "logit")     type <- "OR"
  if (fam$family == "poisson"  && fam$link == "log")       type <- "IRR"
  if (fam$family == "gaussian" && fam$link == "identity")  type <- "b"

  if (is.na(type))
    stop("Model family/link not yet supported by tab_model.glmerMod")

  ## ── fixed-effects parameters ───────────────────────────────────────────────
  pe <- data.table::data.table(
          parameters::parameters(model,
                                 ci_method    = ci_method,
                                 effects      = "fixed",
                                 exponentiate = (type != "b"))
        )
  raw_pe <- data.table::data.table(
              parameters::parameters(model,
                                     ci_method    = ci_method,
                                     effects      = "fixed",
                                     exponentiate = FALSE)
            )

  tab <- if (type != "b") {
           cbind(
             pe[,  .(Parameter, CI_low, CI_high, Coefficient)],
             raw_pe[, .(lnEffect = Coefficient, SE, z, p)]
           )
         } else {
           pe[, .(Parameter, Coefficient, CI_low, CI_high, SE, z, p)]
         }

  ## 95 %-CI string (drop the "glue" class) -----------------------------------
  tab[, est95 := as.character(                                    # ← CHANGED
                    glue::glue_data(
                      .SD,
                      "{papaja::print_num(Coefficient)} ",
                      "[{papaja::print_num(CI_low)}, ",
                      "{papaja::print_num(CI_high)}]"
                    )),
      .SDcols = c("Coefficient", "CI_low", "CI_high")]

# replacement for the final table-assembly block
if (type != "b") {
  out <- tab[,
        {
             .(
           Term     = Parameter,
           est95,
           lnEffect = as.character(papaja::print_num(lnEffect)),
           SE       = as.character(papaja::print_num(SE)),
           z        = as.character(papaja::print_num(z)),
           p        = papaja::print_p(p)
         )
        }
             ]
} else {
  out <- tab[, .(
           Term = Parameter,
           est95,
           SE   = as.character(papaja::print_num(SE)),
           z    = as.character(papaja::print_num(z)),
           p    = papaja::print_p(p)
         )]
}


  names(out)[names(out) == "est95"] <- glue::glue("{type} [95\\% CI]")
  data.table::setnames(out, old = "lnEffect", new = type, skip_absent = TRUE)
  out[]
}

methods::setGeneric("tab_model",
                    function(model, ci_method = NULL,transf = NULL,
                             transf_name = NULL)
                      standardGeneric("tab_model"),
                    signature = "model")

methods::setMethod("tab_model", "lm", tab_model.lm)
methods::setMethod("tab_model", "glm", tab_model.glm)

requireNamespace("glmmTMB")
# glmmTMB is s3, need to set is up as an old s4 class
if (!methods::isClass("glmmTMB"))
  methods::setOldClass("glmmTMB")

methods::setMethod("tab_model", "glmmTMB", tab_model.glmmTMB)
methods::setMethod("tab_model", "lmerMod", tab_model.lmerMod)
methods::setMethod("tab_model", "glmerMod", tab_model.glmerMod)
