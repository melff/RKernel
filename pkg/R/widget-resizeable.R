# resizeable_css <- readLines(system.file("css/resizeable.css",
#                                         package="RKernel"))
# resizeable_esm <- readLines(system.file("js/resizeable.js",
#                                         package="RKernel"))

#' Resizeable Widgets
#' @description R6 classes of resizeable containers for arbitrary HTML code
#' @details Support for resizeable widgets, available from 
#'    \url{https://github.com/melff/resizeable-widget} (and hopefully soon from PyPi.org)
#' @include widget-dom.R
#' @name ResizeableWidget
#' @keywords internal
#' @export 
ResizeableWidgetClass <- R6Class_(
  "ResizeableWidget",
  inherit = AnyWidgetClass,
  public = list(
        #' @field _anywidget_id The AnyWidget id
        `_anywidget_id` = structure(Unicode("ResizeableWidget"), sync = TRUE),
        #' @field _css A Unicode trait with relevant CSS code
        `_css` = structure(Unicode(read_asset("css/resizeable.css")), sync = TRUE),
        #' @field _esm A Unicode trait with relevant Javascript code
        `_esm` = structure(Unicode(read_asset("js/resizeable.js")), sync = TRUE),
        #' @field width (Current) width of the widget
        width = structure(Integer(-1), sync = TRUE),
        #' @field height (Current) height of the widget
        height = structure(Integer(-1), sync = TRUE),
        #' @field direction Which direction(s) should the widget be resizeable
        direction = structure(Unicode("both"), sync = TRUE),
        #' @field value (Current) HTML code content of the widget
        value = structure(Unicode("<em>Hello World!</em>"), sync = TRUE),
        #' @field debug Whether to show a dashed frame and background color for debugging
        debug = structure(Boolean(FALSE), sync = TRUE)
  )
)

#' @rdname ResizeableWidget
VResizeableWidgetClass <- R6Class_(
  "VResizeableWidget",
  inherit = ResizeableWidgetClass,
  public = list(
        #' @field direction Which direction(s) should the widget be resizeable
        direction = structure(Unicode("vertical"), sync = TRUE)
  )
)

#' @rdname ResizeableWidget
HResizeableWidgetClass <- R6Class_(
  "HResizeableWidget",
  inherit = ResizeableWidgetClass,
  public = list(
        #' @field direction Which direction(s) should the widget be resizeable
        direction = structure(Unicode("horizontal"), sync = TRUE)
  )
)

#' @rdname ResizeableWidget
#' @param ... Any arguments, currently ignored
#' @export
ResizeableWidget <- function(...) ResizeableWidgetClass$new(...)

#' @rdname ResizeableWidget
#' @param ... Any arguments, currently ignored
#' @export
VResizeableWidget <- function(...) VResizeableWidgetClass$new(...)

#' @rdname ResizeableWidget
#' @param ... Any arguments, currently ignored
#' @export
HResizeableWidget <- function(...) HResizeableWidgetClass$new(...)
