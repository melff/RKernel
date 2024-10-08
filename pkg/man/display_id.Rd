% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display.R
\name{display_id}
\alias{display_id}
\alias{display_id.display_data}
\alias{display_id.update_display_data}
\title{Get the id of an object display}
\usage{
display_id(x)

\method{display_id}{display_data}(x)

\method{display_id}{update_display_data}(x)
}
\arguments{
\item{x}{An object of class "display_data" or "update_display_data"}
}
\value{
a character string with the id.
}
\description{
This function returns the id of an object created by
    \code{display_data()} or \code{update_display_data()}.
}
\section{Methods (by class)}{
\itemize{
\item \code{display_id(display_data)}: S3 method for "display_data" objects

\item \code{display_id(update_display_data)}: S3 method for "update_display_data" objects

}}
