% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{tab_model.glmerMod}
\alias{tab_model.glmerMod}
\title{tab_model.glmerMod}
\usage{
tab_model.glmerMod(model, ci_method = NULL, transf = NULL, transf_name = NULL)
}
\arguments{
\item{model}{glmerMod object}

\item{ci_method}{confidence interval method send to parameters::parameters}

\item{transf}{optional transformation function}

\item{transf_name}{optional transformation name}
}
\description{
Method for lme4::glmer models (generalised linear mixed models)
}
