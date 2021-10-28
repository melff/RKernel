#' @include widget.R

#' @export
CoreWidgetClass <- R6Class_("CoreWidget",
  inherit = WidgetClass,
  public = list(
    `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
    `_model_name` = structure(Unicode("DOMWidgetModel"),sync=TRUE),
    `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE)
  )
)
