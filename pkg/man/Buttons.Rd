% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widget-button.R
\name{Button}
\alias{Button}
\alias{ButtonClass}
\alias{ButtonStyle}
\alias{ButtonStyleClass}
\alias{Buttons}
\title{Buttons}
\usage{
Button(...)

ButtonStyle(...)
}
\arguments{
\item{...}{Arguments passed to the inializer}
}
\description{
Classes and constructor functions for boxes and box styles
}
\section{Functions}{
\itemize{
\item \code{Button()}: A button constructor

\item \code{ButtonStyle()}: A constructor for a button style

}}
\section{Super classes}{
\code{\link[RKernel:HasTraits]{RKernel::HasTraits}} -> \code{\link[RKernel:Widget]{RKernel::Widget}} -> \code{\link[RKernel:DOMWidget]{RKernel::DOMWidget}} -> \code{Button}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{_model_module}}{Name of the Javascript module with the model}

\item{\code{_model_module_version}}{Version of the module where the model is defined}

\item{\code{_model_name}}{Name of the Javascript model in the frontend}

\item{\code{_view_module}}{Name of the module where the view is defined}

\item{\code{_view_module_version}}{Version of the module where the view is defined}

\item{\code{_view_name}}{Name of the Javascript model view in the frontend}

\item{\code{description}}{A button description}

\item{\code{disabled}}{Boolean, whether the button is disabled}

\item{\code{icon}}{Name of an optional icon}

\item{\code{button_style}}{The string that describes the button style}

\item{\code{tooltip}}{An optional tooltip string}

\item{\code{style}}{The button style, an object of class "ButtonStyleClass"}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Button-on_click}{\code{ButtonClass$on_click()}}
\item \href{#method-Button-click}{\code{ButtonClass$click()}}
\item \href{#method-Button-clone}{\code{ButtonClass$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="notify"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-notify'><code>RKernel::HasTraits$notify()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="observe"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-observe'><code>RKernel::HasTraits$observe()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="validate"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-validate'><code>RKernel::HasTraits$validate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="_send"><a href='../../RKernel/html/Widget.html#method-Widget-_send'><code>RKernel::Widget$_send()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="check_version"><a href='../../RKernel/html/Widget.html#method-Widget-check_version'><code>RKernel::Widget$check_version()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="close"><a href='../../RKernel/html/Widget.html#method-Widget-close'><code>RKernel::Widget$close()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="display_data"><a href='../../RKernel/html/Widget.html#method-Widget-display_data'><code>RKernel::Widget$display_data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="get_state"><a href='../../RKernel/html/Widget.html#method-Widget-get_state'><code>RKernel::Widget$get_state()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_buffers"><a href='../../RKernel/html/Widget.html#method-Widget-handle_buffers'><code>RKernel::Widget$handle_buffers()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_comm_msg"><a href='../../RKernel/html/Widget.html#method-Widget-handle_comm_msg'><code>RKernel::Widget$handle_comm_msg()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_comm_opened"><a href='../../RKernel/html/Widget.html#method-Widget-handle_comm_opened'><code>RKernel::Widget$handle_comm_opened()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_custom_msg"><a href='../../RKernel/html/Widget.html#method-Widget-handle_custom_msg'><code>RKernel::Widget$handle_custom_msg()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_displayed"><a href='../../RKernel/html/Widget.html#method-Widget-handle_displayed'><code>RKernel::Widget$handle_displayed()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_event"><a href='../../RKernel/html/Widget.html#method-Widget-handle_event'><code>RKernel::Widget$handle_event()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="initialize"><a href='../../RKernel/html/Widget.html#method-Widget-initialize'><code>RKernel::Widget$initialize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="on_displayed"><a href='../../RKernel/html/Widget.html#method-Widget-on_displayed'><code>RKernel::Widget$on_displayed()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="on_event"><a href='../../RKernel/html/Widget.html#method-Widget-on_event'><code>RKernel::Widget$on_event()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="on_msg"><a href='../../RKernel/html/Widget.html#method-Widget-on_msg'><code>RKernel::Widget$on_msg()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="open"><a href='../../RKernel/html/Widget.html#method-Widget-open'><code>RKernel::Widget$open()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="send"><a href='../../RKernel/html/Widget.html#method-Widget-send'><code>RKernel::Widget$send()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="send_state"><a href='../../RKernel/html/Widget.html#method-Widget-send_state'><code>RKernel::Widget$send_state()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="set_state"><a href='../../RKernel/html/Widget.html#method-Widget-set_state'><code>RKernel::Widget$set_state()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="DOMWidget" data-id="add_class"><a href='../../RKernel/html/DOMWidget.html#method-DOMWidget-add_class'><code>RKernel::DOMWidget$add_class()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="DOMWidget" data-id="has_class"><a href='../../RKernel/html/DOMWidget.html#method-DOMWidget-has_class'><code>RKernel::DOMWidget$has_class()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="DOMWidget" data-id="remove_class"><a href='../../RKernel/html/DOMWidget.html#method-DOMWidget-remove_class'><code>RKernel::DOMWidget$remove_class()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Button-on_click"></a>}}
\if{latex}{\out{\hypertarget{method-Button-on_click}{}}}
\subsection{Method \code{on_click()}}{
Add or remove a click handler
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ButtonClass$on_click(handler, remove = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{handler}}{A function that is called when the button is clicked}

\item{\code{remove}}{Logical value, whether the handler is to be removed}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Button-click"></a>}}
\if{latex}{\out{\hypertarget{method-Button-click}{}}}
\subsection{Method \code{click()}}{
Function that calls the click event handlers
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ButtonClass$click()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Button-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Button-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ButtonClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
\section{Super classes}{
\code{\link[RKernel:HasTraits]{RKernel::HasTraits}} -> \code{\link[RKernel:Widget]{RKernel::Widget}} -> \code{\link[RKernel:DescriptionStyle]{RKernel::DescriptionStyle}} -> \code{ButtonStyle}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{_model_name}}{Name of the Javascript model in the frontend}

\item{\code{button_color}}{The colour of the button}

\item{\code{font_family}}{The font family of the button label}

\item{\code{font_size}}{The font size of the button label}

\item{\code{font_style}}{The font style of the button label}

\item{\code{font_variant}}{The font variant of the button label}

\item{\code{font_weight}}{The font weight of the button label}

\item{\code{text_color}}{The text colour of the button label}

\item{\code{text_decoration}}{The text decoration of the button label}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ButtonStyle-clone}{\code{ButtonStyleClass$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="notify"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-notify'><code>RKernel::HasTraits$notify()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="observe"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-observe'><code>RKernel::HasTraits$observe()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="HasTraits" data-id="validate"><a href='../../RKernel/html/HasTraits.html#method-HasTraits-validate'><code>RKernel::HasTraits$validate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="_send"><a href='../../RKernel/html/Widget.html#method-Widget-_send'><code>RKernel::Widget$_send()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="check_version"><a href='../../RKernel/html/Widget.html#method-Widget-check_version'><code>RKernel::Widget$check_version()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="close"><a href='../../RKernel/html/Widget.html#method-Widget-close'><code>RKernel::Widget$close()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="display_data"><a href='../../RKernel/html/Widget.html#method-Widget-display_data'><code>RKernel::Widget$display_data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="get_state"><a href='../../RKernel/html/Widget.html#method-Widget-get_state'><code>RKernel::Widget$get_state()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_buffers"><a href='../../RKernel/html/Widget.html#method-Widget-handle_buffers'><code>RKernel::Widget$handle_buffers()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_comm_msg"><a href='../../RKernel/html/Widget.html#method-Widget-handle_comm_msg'><code>RKernel::Widget$handle_comm_msg()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_comm_opened"><a href='../../RKernel/html/Widget.html#method-Widget-handle_comm_opened'><code>RKernel::Widget$handle_comm_opened()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_custom_msg"><a href='../../RKernel/html/Widget.html#method-Widget-handle_custom_msg'><code>RKernel::Widget$handle_custom_msg()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_displayed"><a href='../../RKernel/html/Widget.html#method-Widget-handle_displayed'><code>RKernel::Widget$handle_displayed()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="handle_event"><a href='../../RKernel/html/Widget.html#method-Widget-handle_event'><code>RKernel::Widget$handle_event()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="initialize"><a href='../../RKernel/html/Widget.html#method-Widget-initialize'><code>RKernel::Widget$initialize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="on_displayed"><a href='../../RKernel/html/Widget.html#method-Widget-on_displayed'><code>RKernel::Widget$on_displayed()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="on_event"><a href='../../RKernel/html/Widget.html#method-Widget-on_event'><code>RKernel::Widget$on_event()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="on_msg"><a href='../../RKernel/html/Widget.html#method-Widget-on_msg'><code>RKernel::Widget$on_msg()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="open"><a href='../../RKernel/html/Widget.html#method-Widget-open'><code>RKernel::Widget$open()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="send"><a href='../../RKernel/html/Widget.html#method-Widget-send'><code>RKernel::Widget$send()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="send_state"><a href='../../RKernel/html/Widget.html#method-Widget-send_state'><code>RKernel::Widget$send_state()</code></a></span></li>
<li><span class="pkg-link" data-pkg="RKernel" data-topic="Widget" data-id="set_state"><a href='../../RKernel/html/Widget.html#method-Widget-set_state'><code>RKernel::Widget$set_state()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ButtonStyle-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ButtonStyle-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ButtonStyleClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
