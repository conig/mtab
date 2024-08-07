% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{tab}
\alias{tab}
\title{tab}
\usage{
tab(model, ci_method = NULL, transf = NULL, transf_name = NULL, replace = NULL)
}
\arguments{
\item{model}{model to tabulate}

\item{ci_method}{passed to parameters}

\item{transf}{function to transform coefficients (not SE)}

\item{transf_name}{name for transformed coefficient}

\item{replace}{named vector, names replaced with contents}
}
\description{
Assemble model info into data.frame
}
