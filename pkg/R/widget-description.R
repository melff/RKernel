#' @include widget.R widget-dom.R

#' @export
DescriptionStyleClass <- R6Class_("DescriptionStyle",
  inherit = WidgetClass,
  public = list(
    `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
    `_model_name` = structure(Unicode("DescriptionStyleModel"),sync=TRUE),
    `_view_name` = structure(Unicode("StyleView"),sync=TRUE),
    `_view_module` = structure(Unicode("@jupyter-widgets/base"),sync=TRUE),
    `_view_module_version` = structure(Unicode(jupyter_widgets_base_version),sync=TRUE),
    description_width = structure(Unicode(character(0)),sync=TRUE)
  )
)
#' @export
DescriptionStyle <- function(...) DescriptionStyleClass$new(...)

#' @export
DescriptionWidgetClass <- R6Class_("DescriptionWidget",
  inherit = DOMWidgetClass,
  public = list(
    `_model_name` = structure(Unicode("DescriptionModel"),sync=TRUE),
    description = structure(Unicode(""),sync=TRUE),
    description_tooltip = structure(Unicode(""),sync=TRUE),
    style = structure(R6Instance(DescriptionStyleClass),sync=TRUE)
  )
)

#' @export
DescriptionWidget <- function(...) DescriptionWidgetClass$new(...)
