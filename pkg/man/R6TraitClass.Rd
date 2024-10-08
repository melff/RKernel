% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traitlets-R6instance.R
\name{R6TraitClass}
\alias{R6TraitClass}
\title{A Base Class for Traits that are R6 Objects}
\description{
A Base Class for Traits that are R6 Objects

A Base Class for Traits that are R6 Objects
}
\section{Super class}{
\code{\link[RKernel:Trait]{RKernel::Trait}} -> \code{R6Trait}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{value}}{An R6 object}

\item{\code{class}}{The R6 class of \code{value}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-R6Trait-validator}{\code{R6TraitClass$validator()}}
\item \href{#method-R6Trait-new}{\code{R6TraitClass$new()}}
\item \href{#method-R6Trait-clone}{\code{R6TraitClass$clone()}}
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
\if{html}{\out{<a id="method-R6Trait-validator"></a>}}
\if{latex}{\out{\hypertarget{method-R6Trait-validator}{}}}
\subsection{Method \code{validator()}}{
Checks wether \code{value} has the corret class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{R6TraitClass$validator(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{A value about to be assigned to the trait.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-R6Trait-new"></a>}}
\if{latex}{\out{\hypertarget{method-R6Trait-new}{}}}
\subsection{Method \code{new()}}{
Initialize an object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{R6TraitClass$new(Class, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{Class}}{Class of the object}

\item{\code{...}}{Values used for initialization}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-R6Trait-clone"></a>}}
\if{latex}{\out{\hypertarget{method-R6Trait-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{R6TraitClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
