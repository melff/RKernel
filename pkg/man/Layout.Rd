% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widget-layout.R
\name{Layout}
\alias{Layout}
\alias{LayoutClass}
\title{Widget Layout Manipulation}
\usage{
Layout(...)
}
\arguments{
\item{...}{Arguments passed to the inializer}
}
\description{
An R6 class and a constructor function for the creation of
    a layout widget, which itself is used to manipulate the layout of
    a \code{\link{DOMWidget}}.
}
\details{
The function \code{Layout} creates objects of the R6 Class
    "LayoutClass", which in turn have the S3 class attribute "Layout"
}
\section{Functions}{
\itemize{
\item \code{Layout()}: The Layout constructor function

}}
\section{Super classes}{
\code{\link[RKernel:HasTraits]{RKernel::HasTraits}} -> \code{\link[RKernel:Widget]{RKernel::Widget}} -> \code{Layout}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{_view_name}}{Name of the Javascript view in the frontend.}

\item{\code{_view_module}}{Name of the Javascript view module in the frontend.}

\item{\code{_view_module_version}}{Version of the Javascript view module in the frontend.}

\item{\code{_model_name}}{Name of the Javascript model in the frontend.}

\item{\code{align_content}}{An optional string, if non-empty, one of "flex-start", "flex-end",
"center", "space-between", "space-around", "space-evenly", "stretch"}

\item{\code{align_items}}{An optional string, if non-empty, one of "flex-start", "flex-end",
"center", "baseline", "stretch"}

\item{\code{align_self}}{An optional string, if non-empty, one of "flex-start", "flex-end",
"center", "baseline", "stretch"}

\item{\code{bottom}}{Position from bottom, an optional string that should, if non-empty, contain a valid CSS dimension}

\item{\code{border}}{An optional string with a valid CSS border specification}

\item{\code{border_top}}{An optional string with a valid CSS border specification}

\item{\code{border_right}}{An optional string with a valid CSS border specification}

\item{\code{border_bottom}}{An optional string with a valid CSS border specification}

\item{\code{border_left}}{An optional string with a valid CSS border specification}

\item{\code{display}}{An optional string with a valid CSS display property}

\item{\code{flex}}{An optional string with a valid CSS flex property}

\item{\code{flex_flow}}{An optional string with a valid CSS flex_flow property}

\item{\code{height}}{An optional string with a valid CSS height}

\item{\code{justify_content}}{An optional string, if non-empty, one of "flex-start", "flex-end",
"center", "space-between", "space-around".}

\item{\code{justify_items}}{An optional string, if non-empty, one of "flex-start", "flex-end",
or "center"}

\item{\code{left}}{Position from left, an optional string that should, if non-empty, contain a valid CSS dimension}

\item{\code{margin}}{An optional string, if non-empty, should be a valid CSS margin specification}

\item{\code{max_height}}{An optional string, if non-emtpy, should be a valid CSS dimension}

\item{\code{max_width}}{An optional string, if non-emtpy, should be a valid CSS dimension}

\item{\code{min_height}}{An optional string, if non-emtpy, should be a valid CSS dimension}

\item{\code{min_width}}{An optional string, if non-emtpy, should be a valid CSS dimension}

\item{\code{overflow}}{An optonal string, if non-empty, should be a valid CSS overflow specification}

\item{\code{order}}{An optional string, if non-empty should contain a number}

\item{\code{padding}}{An optional string, if non-emtpy should be a valid CSS dimension}

\item{\code{right}}{Position from right, an optional string, if non-empty, should be a valid CSS dimension}

\item{\code{top}}{Position from top, an optional string, if non-empty, should be a valid CSS dimension}

\item{\code{visibility}}{An optional string, if non-empty, should be either "visible" or "hidden"}

\item{\code{width}}{An optional string, if non-empty, should be a valid CSS dimension}

\item{\code{object_fit}}{An optional string, if non-empty, should be one of "contain", "cover",
"fill", "scale-down", "none"}

\item{\code{object_position}}{An optional string, if non-empty, should be a valid CSS object-position specification}

\item{\code{grid_auto_columns}}{An optional string, if non-empty should be valid CSS code for the grid-auto-columns 
property}

\item{\code{grid_auto_flow}}{An optional string, if non-empty should be valid CSS code for the grid-auto-flow
property}

\item{\code{grid_auto_rows}}{An optional string, if non-empty should be valid CSS code for the grid-auto-rows 
property}

\item{\code{grid_gap}}{An optional string, if non-empty should be valid CSS code for the grid-gap 
property}

\item{\code{grid_template_rows}}{An optional string, if non-empty should be valid CSS code for the
grid-template-rows property}

\item{\code{grid_template_columns}}{An optional string, if non-empty should be valid CSS code for the
grid-template-columns property}

\item{\code{grid_template_areas}}{An optional string, if non-empty should be valid CSS code for the
grid-template-areas property}

\item{\code{grid_row}}{An optional string, if non-empty should be valid CSS code for the
grid-row property}

\item{\code{grid_column}}{An optional string, if non-empty should be valid CSS code for the
grid-column property}

\item{\code{grid_area}}{An optional string, if non-empty should be valid CSS code for the
grid-area property}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Layout-observe_border}{\code{LayoutClass$observe_border()}}
\item \href{#method-Layout-new}{\code{LayoutClass$new()}}
\item \href{#method-Layout-clone}{\code{LayoutClass$clone()}}
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
\if{html}{\out{<a id="method-Layout-observe_border"></a>}}
\if{latex}{\out{\hypertarget{method-Layout-observe_border}{}}}
\subsection{Method \code{observe_border()}}{
Synchronize border traits
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LayoutClass$observe_border(nm, self, value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{nm}}{Name of the trait (a dummy argument)}

\item{\code{self}}{The object}

\item{\code{value}}{A CSS string}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Layout-new"></a>}}
\if{latex}{\out{\hypertarget{method-Layout-new}{}}}
\subsection{Method \code{new()}}{
Initialize an object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LayoutClass$new(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to the superclass initializer}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Layout-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Layout-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LayoutClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
