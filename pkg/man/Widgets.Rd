% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widget.R
\name{Widgets}
\alias{Widgets}
\alias{WidgetClass}
\alias{Widget}
\title{A Widget Base Class}
\usage{
Widget(...)
}
\arguments{
\item{...}{Arguments passed to the inializer}
}
\description{
The base class from which all widget classes are derived
}
\section{Functions}{
\itemize{
\item \code{Widget()}: A Widget Constructor Function

}}
\section{Super class}{
\code{\link[RKernel:HasTraits]{RKernel::HasTraits}} -> \code{Widget}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{_model_id}}{Identifyer of the frontend Javascript object}

\item{\code{_model_name}}{Name of the Javascript model in the frontend}

\item{\code{_model_module}}{Name of the Javascript module with the model}

\item{\code{_model_module_version}}{Version of the module where the model is defined}

\item{\code{_view_name}}{Name of the Javascript model view in the frontend}

\item{\code{_view_module}}{Version of the module where the view is defined}

\item{\code{_view_module_version}}{Version of the module where the view is defined}

\item{\code{_view_count}}{Number of views that refer to the same frontend model object}

\item{\code{traits_to_sync}}{Names of the traits to be synchronized with the frontend}

\item{\code{sync_suspended}}{Logical value, whether synchronization is suspended}

\item{\code{custom_msg_callbacks}}{A list of functions to be called on receiving a message}

\item{\code{event_callbacks}}{A list of functions to be called on an event}

\item{\code{displayed_callbacks}}{A list of functions to be called when the widget}

\item{\code{_comm}}{The 'comm' connecting to the frontend or NULL}

\item{\code{required_version}}{Minimum required ipywidgets version in which the
current widget class is supported.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{comm}}{The 'comm' connecting the frontend (as an active binding)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Widget-new}{\code{WidgetClass$new()}}
\item \href{#method-Widget-open}{\code{WidgetClass$open()}}
\item \href{#method-Widget-close}{\code{WidgetClass$close()}}
\item \href{#method-Widget-get_state}{\code{WidgetClass$get_state()}}
\item \href{#method-Widget-set_state}{\code{WidgetClass$set_state()}}
\item \href{#method-Widget-send_state}{\code{WidgetClass$send_state()}}
\item \href{#method-Widget-send}{\code{WidgetClass$send()}}
\item \href{#method-Widget-display_data}{\code{WidgetClass$display_data()}}
\item \href{#method-Widget-handle_comm_opened}{\code{WidgetClass$handle_comm_opened()}}
\item \href{#method-Widget-handle_comm_msg}{\code{WidgetClass$handle_comm_msg()}}
\item \href{#method-Widget-handle_buffers}{\code{WidgetClass$handle_buffers()}}
\item \href{#method-Widget-handle_custom_msg}{\code{WidgetClass$handle_custom_msg()}}
\item \href{#method-Widget-on_msg}{\code{WidgetClass$on_msg()}}
\item \href{#method-Widget-on_event}{\code{WidgetClass$on_event()}}
\item \href{#method-Widget-handle_event}{\code{WidgetClass$handle_event()}}
\item \href{#method-Widget-on_displayed}{\code{WidgetClass$on_displayed()}}
\item \href{#method-Widget-handle_displayed}{\code{WidgetClass$handle_displayed()}}
\item \href{#method-Widget-_send}{\code{WidgetClass$_send()}}
\item \href{#method-Widget-check_version}{\code{WidgetClass$check_version()}}
\item \href{#method-Widget-clone}{\code{WidgetClass$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="notify"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-notify'><code>RKernel::HasTraits$notify()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="observe"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-observe'><code>RKernel::HasTraits$observe()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="validate"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-validate'><code>RKernel::HasTraits$validate()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-new"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-new}{}}}
\subsection{Method \code{new()}}{
Initialize an object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$new(..., open = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Values used for initialization}

\item{\code{open}}{Logical, whether a connection with the frontend should be opened}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-open"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-open}{}}}
\subsection{Method \code{open()}}{
Open a connection to the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$open()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-close"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-close}{}}}
\subsection{Method \code{close()}}{
Close the connection to the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$close()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-get_state"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-get_state}{}}}
\subsection{Method \code{get_state()}}{
Prepare synchronized traits for sending them to the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$get_state(keys = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{keys}}{Keys/names of the traits to be updated in the frontend}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-set_state"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-set_state}{}}}
\subsection{Method \code{set_state()}}{
Update the synchronized states, usually with information from the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$set_state(state)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{state}}{A list of values for the synchronized traits}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-send_state"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-send_state}{}}}
\subsection{Method \code{send_state()}}{
Send updated traits to the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$send_state(keys = NULL, drop_defaults = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{keys}}{Keys/names of the traits to be updated in the frontend}

\item{\code{drop_defaults}}{Logical value, not yet used}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-send"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-send}{}}}
\subsection{Method \code{send()}}{
Send content and binary buffers to the fronend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$send(content, buffers = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{content}}{Some user-defined information to be send to the frontend}

\item{\code{buffers}}{Some raw vector buffers}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-display_data"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-display_data}{}}}
\subsection{Method \code{display_data()}}{
Send display-data of the widget to the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$display_data()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-handle_comm_opened"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-handle_comm_opened}{}}}
\subsection{Method \code{handle_comm_opened()}}{
Handle a 'comm' opened in the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$handle_comm_opened(comm, data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{comm}}{The 'comm' object that is opened}

\item{\code{data}}{Data sent by the frontend}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-handle_comm_msg"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-handle_comm_msg}{}}}
\subsection{Method \code{handle_comm_msg()}}{
Handle a message from the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$handle_comm_msg(comm, msg)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{comm}}{The 'comm' object via which the message is received}

\item{\code{msg}}{Message sent by the frontend}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-handle_buffers"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-handle_buffers}{}}}
\subsection{Method \code{handle_buffers()}}{
Handle buffers in message.
  This method should be overwritten by inherting classes that actually
  process data in buffer components of messages.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$handle_buffers(msg)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{msg}}{A comm message}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-handle_custom_msg"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-handle_custom_msg}{}}}
\subsection{Method \code{handle_custom_msg()}}{
Call the custom message handlers
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$handle_custom_msg(content)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{content}}{The data received}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-on_msg"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-on_msg}{}}}
\subsection{Method \code{on_msg()}}{
Install a handler for messages being received
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$on_msg(handler, remove = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{handler}}{A handler function}

\item{\code{remove}}{Logical, should the handler be removed?}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-on_event"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-on_event}{}}}
\subsection{Method \code{on_event()}}{
Install a handler for events in the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$on_event(event, handler, remove = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{event}}{A character that describes the event}

\item{\code{handler}}{A handler function}

\item{\code{remove}}{Logical, should the handler be removed?}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-handle_event"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-handle_event}{}}}
\subsection{Method \code{handle_event()}}{
Call the installed event handlers
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$handle_event(event, args)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{event}}{A string that describes the event}

\item{\code{args}}{A list of argument passed on with the event}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-on_displayed"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-on_displayed}{}}}
\subsection{Method \code{on_displayed()}}{
Install a handler to be called when the widget is displayed
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$on_displayed(handler, remove = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{handler}}{A handler function}

\item{\code{remove}}{Logical, should the handler be removed?}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-handle_displayed"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-handle_displayed}{}}}
\subsection{Method \code{handle_displayed()}}{
Call the installed display handlers
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$handle_displayed()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-_send"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-_send}{}}}
\subsection{Method \code{_send()}}{
The internal function to send messages to the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$_send(msg, buffers = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{msg}}{The message}

\item{\code{buffers}}{Raw data buffers or NULL}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-check_version"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-check_version}{}}}
\subsection{Method \code{check_version()}}{
Check whether current widget class is supported by ipywidgets
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$check_version()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Widget-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Widget-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WidgetClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
