% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help.R
\name{help.start}
\alias{help.start}
\title{Start interactive help system}
\usage{
help.start(
  update = FALSE,
  gui = "irrelevant",
  browser = getOption("browser"),
  remote = NULL
)
}
\arguments{
\item{update}{A logical value. This formal argument exists for compatibility
reasons only.}

\item{gui}{A character string. This formal argument exists for compatibility
reasons only.}

\item{browser}{A character string. This formal argument exists for compatibility
reasons only.}

\item{remote}{A character string. This formal argument exists for compatibility
reasons only.}
}
\description{
A variant of \code{\link[utils]{help.start}} that works when called from inside a
    Jupyter notebook.
}
