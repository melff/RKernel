% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traitlets-boolean.R
\name{Boolean}
\alias{Boolean}
\alias{BooleanClass}
\title{Boolean Traitlets}
\usage{
Boolean(...)
}
\arguments{
\item{...}{Arguments that are passed to the initialize method of 'BooleanClass'}
}
\description{
A class and a constructor function to create boolean trait(let)s.
}
\section{Super class}{
\code{\link[RKernel:Trait]{RKernel::Trait}} -> \code{Boolean}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{value}}{A logical vector, usually of length 1}

\item{\code{optional}}{Logical, whether an initializing logical value must be provided}

\item{\code{coerce}}{Logical, whether 'as.logical()' is implicitely used when a value is
assigned to the trait}

\item{\code{length}}{Integer, the length of the logical vector that poses as the value of the traitlet.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Boolean-validator}{\code{BooleanClass$validator()}}
\item \href{#method-Boolean-new}{\code{BooleanClass$new()}}
\item \href{#method-Boolean-clone}{\code{BooleanClass$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Trait" data-id="get"><a href='../../RKernel/html/Trait.html#method-Trait-get'><code>RKernel::Trait$get()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Trait" data-id="set"><a href='../../RKernel/html/Trait.html#method-Trait-set'><code>RKernel::Trait$set()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Boolean-validator"></a>}}
\if{latex}{\out{\hypertarget{method-Boolean-validator}{}}}
\subsection{Method \code{validator()}}{
A validator method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BooleanClass$validator(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{A value to be checked for validity}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Boolean-new"></a>}}
\if{latex}{\out{\hypertarget{method-Boolean-new}{}}}
\subsection{Method \code{new()}}{
The initializing method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BooleanClass$new(
  initial,
  coerce = TRUE,
  optional = length(initial) == 0,
  length = 1
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{initial}}{A value with which the traitlet is initialized}

\item{\code{coerce}}{Logical, used to initialize the 'coerce' field}

\item{\code{optional}}{Logical, used to initialize the 'optional' field}

\item{\code{length}}{Integer, used to initialize the 'length' field}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Boolean-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Boolean-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BooleanClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
