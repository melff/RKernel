% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/json.R, R/traitlets.R, R/traitlets-vector.R,
%   R/widget.R, R/traitlets-bytes.R
\name{to_json}
\alias{to_json}
\alias{to_json.default}
\alias{to_json.list}
\alias{to_json.json}
\alias{to_json.Trait}
\alias{to_json.Vector}
\alias{to_json.List}
\alias{to_json.Widget}
\alias{to_json.Bytes}
\title{A Genereric Converter to JSON}
\usage{
to_json(x, auto_unbox = TRUE, ...)

\method{to_json}{default}(x, auto_unbox = TRUE, ...)

\method{to_json}{list}(x, auto_unbox = TRUE, ...)

\method{to_json}{json}(x, ...)

\method{to_json}{Trait}(x, auto_unbox = TRUE, ...)

\method{to_json}{Vector}(x, ...)

\method{to_json}{List}(x, auto_unbox = TRUE, ...)

\method{to_json}{Widget}(x, ...)

\method{to_json}{Bytes}(x, ...)
}
\arguments{
\item{x}{An object to be converted as JSON}

\item{auto_unbox}{A logical value, whether one-element a JSON list should be
changed into JSON scalar.}

\item{...}{Other arguments, passed on to  \code{\link[jsonlite]{toJSON}}.}
}
\description{
The function \code{to_json} is a generic and idempotent interface
    to \code{\link[jsonlite]{toJSON}}.
}
\section{Methods (by class)}{
\itemize{
\item \code{to_json(default)}: Default S3 method

\item \code{to_json(list)}: S3 method for lists.

\item \code{to_json(json)}: S3 method for JSON character strings. Returns its
argument as is, making \code{to_json} idempotent.

\item \code{to_json(Trait)}: S3 method for 'TraitClass' objects, i.e. traitlets.

\item \code{to_json(Vector)}: S3 method for 'VectorClass' objects

\item \code{to_json(List)}: S3 method for 'ListClass' objects

\item \code{to_json(Widget)}: S3 method for 'WidgetClass' objects, i.e. jupyter widgets

\item \code{to_json(Bytes)}: S3 method for 'BytesClass' objects

}}
