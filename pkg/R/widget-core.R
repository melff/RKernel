#' @export
CoreWidgetClass <- R6Class("CoreWidget",
  inherit = WidgetClass,
  public = list(
    `_model_module` = trait("@jupyter-widgets/controls",sync=TRUE),
    `_model_module_version` = trait(jupyter_widgets_controls_version,sync=TRUE),
    `_model_name` = trait("DOMWidgetModel",sync=TRUE),
    `_view_module` = trait("@jupyter-widgets/controls",sync=TRUE),
    `_view_module_version` = trait(jupyter_widgets_controls_version,sync=TRUE)
  )
)
