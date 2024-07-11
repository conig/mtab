

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

