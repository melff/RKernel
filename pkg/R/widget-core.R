#' @export
CoreWidgetClass <- R6Class_("CoreWidget",
  inherit = WidgetClass,
  public = list(
    `_model_module` = Unicode("@jupyter-widgets/controls",sync=TRUE),
    `_model_module_version` =Unicode(jupyter_widgets_controls_version,sync=TRUE),
    `_model_name` = Unicode("DOMWidgetModel",sync=TRUE),
    `_view_module` = Unicode("@jupyter-widgets/controls",sync=TRUE),
    `_view_module_version` = Unicode(jupyter_widgets_controls_version,sync=TRUE)
  )
)
