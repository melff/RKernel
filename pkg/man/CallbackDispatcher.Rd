% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callbacks.R
\name{CallbackDispatcher}
\alias{CallbackDispatcher}
\alias{CallbackDispatcherClass}
\title{A Dispatcher for Callbacks}
\usage{
CallbackDispatcher(...)
}
\arguments{
\item{...}{Arguments passed to the inializer}
}
\description{
Objects in this class are collections of callbacks functions
    usually related to certain events. The function \code{CallbackDispachter} can be
    used as an constructor
}
\section{Functions}{
\itemize{
\item \code{CallbackDispatcher()}: The constructor function, returns an Object of Class "CallbackDispatcherClass"

}}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CallbackDispatcher-register}{\code{CallbackDispatcherClass$register()}}
\item \href{#method-CallbackDispatcher-clear}{\code{CallbackDispatcherClass$clear()}}
\item \href{#method-CallbackDispatcher-suspend_handlers}{\code{CallbackDispatcherClass$suspend_handlers()}}
\item \href{#method-CallbackDispatcher-activate_handlers}{\code{CallbackDispatcherClass$activate_handlers()}}
\item \href{#method-CallbackDispatcher-run}{\code{CallbackDispatcherClass$run()}}
\item \href{#method-CallbackDispatcher-clone}{\code{CallbackDispatcherClass$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CallbackDispatcher-register"></a>}}
\if{latex}{\out{\hypertarget{method-CallbackDispatcher-register}{}}}
\subsection{Method \code{register()}}{
Register a function as a callback
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CallbackDispatcherClass$register(handler, remove)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{handler}}{A function}

\item{\code{remove}}{A logical value; whether the function
is added or removed from the list of callbacks}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CallbackDispatcher-clear"></a>}}
\if{latex}{\out{\hypertarget{method-CallbackDispatcher-clear}{}}}
\subsection{Method \code{clear()}}{
Remove all callback functions
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CallbackDispatcherClass$clear()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CallbackDispatcher-suspend_handlers"></a>}}
\if{latex}{\out{\hypertarget{method-CallbackDispatcher-suspend_handlers}{}}}
\subsection{Method \code{suspend_handlers()}}{
Suspend registered callback functions
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CallbackDispatcherClass$suspend_handlers()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CallbackDispatcher-activate_handlers"></a>}}
\if{latex}{\out{\hypertarget{method-CallbackDispatcher-activate_handlers}{}}}
\subsection{Method \code{activate_handlers()}}{
(Re-)activate registered callback functions
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CallbackDispatcherClass$activate_handlers()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CallbackDispatcher-run"></a>}}
\if{latex}{\out{\hypertarget{method-CallbackDispatcher-run}{}}}
\subsection{Method \code{run()}}{
Run all registered callback functions
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CallbackDispatcherClass$run(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Aruments passed on to the handler functions}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CallbackDispatcher-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CallbackDispatcher-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CallbackDispatcherClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
