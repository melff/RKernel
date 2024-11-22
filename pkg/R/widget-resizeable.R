#' Resizeable Widgets
#' @description R6 classes of resizeable containers for arbitrary HTML code
#' @include widget-dom.R
#' @name ResizeableWidget
#' @export 
ResizeableWidgetClass <- R6Class_(
  "ResizeableWidget",
  inherit = DOMWidgetClass,
  public = list(
          #' @field _view_name Name of the Javascript model view in the frontend
        `_view_name` = structure(Unicode("ResizeableView"),sync=TRUE),
        #' @field _model_name Name of the Javascript model in the frontend
        `_model_name` = structure(Unicode("ResizeableModel"),sync=TRUE),
        #' @field _view_module Name of the module where the view is defined
        `_view_module` = structure(Unicode("resizeable-widget"),sync=TRUE),
        #' @field _model_module Name of the Javascript module with the model
        `_model_module` = structure(Unicode("resizeable-widget"),sync=TRUE),
        #' @field _view_module_version Version of the module where the view is defined
        `_view_module_version` = structure(Unicode("^0.1.0"),sync=TRUE),
        #' @field _model_module_version Version of the module where the model is defined
        `_model_module_version` = structure(Unicode("^0.1.0"),sync=TRUE),
        #' @field width (Current) width of the widget
        width = structure(Integer(-1),sync = TRUE),
        #' @field height (Current) height of the widget
        height = structure(Integer(-1),sync = TRUE),
        #' @field value (Current) HTML code content of the widget
        value = structure(Unicode("<em>Hello World!</em>"),sync = TRUE),
        #' @field debug Whether to show a dashed frame and background color for debugging
        debug = structure(Boolean(FALSE),sync = TRUE)
  )
)

#' @rdname ResizeableWidget
VResizeableWidgetClass <- R6Class_(
  "VResizeableWidget",
  inherit = ResizeableWidgetClass,
  public = list(
          #' @field _view_name Name of the Javascript model view in the frontend
        `_view_name` = structure(Unicode("VResizeableView"),sync=TRUE),
        #' @field _model_name Name of the Javascript model in the frontend
        `_model_name` = structure(Unicode("VResizeableModel"),sync=TRUE)
  )
)

#' @rdname ResizeableWidget
HResizeableWidgetClass <- R6Class_(
  "HResizeableWidget",
  inherit = ResizeableWidgetClass,
  public = list(
          #' @field _view_name Name of the Javascript model view in the frontend
        `_view_name` = structure(Unicode("HResizeableView"),sync=TRUE),
        #' @field _model_name Name of the Javascript model in the frontend
        `_model_name` = structure(Unicode("HResizeableModel"),sync=TRUE)
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
