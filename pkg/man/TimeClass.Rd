% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traitlets-datetime.R
\name{TimeClass}
\alias{TimeClass}
\title{Time Traitlets}
\description{
A class and constructor of time traitlets.
}
\section{Super class}{
\code{\link[RKernel:Trait]{RKernel::Trait}} -> \code{TimeClass}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{value}}{A date.}

\item{\code{coerce}}{Logical value, whether assignments to the value field should
be coerced to the appropriate type.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TimeClass-validator}{\code{TimeClass$validator()}}
\item \href{#method-TimeClass-new}{\code{TimeClass$new()}}
\item \href{#method-TimeClass-clone}{\code{TimeClass$clone()}}
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
\if{html}{\out{<a id="method-TimeClass-validator"></a>}}
\if{latex}{\out{\hypertarget{method-TimeClass-validator}{}}}
\subsection{Method \code{validator()}}{
Check the value assigned to the traitlet.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TimeClass$validator(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{The value assigned to the traitlet.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TimeClass-new"></a>}}
\if{latex}{\out{\hypertarget{method-TimeClass-new}{}}}
\subsection{Method \code{new()}}{
Initialize the traitlet.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TimeClass$new(initial = as.POSIXct(integer(0)), coerce = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{initial}}{An optional POSIXct object or an object coercive into such an object}

\item{\code{coerce}}{An optional logical value}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TimeClass-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TimeClass-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TimeClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
