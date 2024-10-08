% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display.R
\name{raw_html}
\alias{raw_html}
\title{Send raw HTML code to the frontend}
\usage{
raw_html(text, id = UUIDgenerate(), update = FALSE)
}
\arguments{
\item{text}{A character string with LaTeX code for math}

\item{id}{A character string with the display id}

\item{update}{A logical value, should an existing display_data option?}
}
\description{
Send raw HTML code in a character string to the
    frontend
}
