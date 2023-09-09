#' Yet Another Widget Class
#' @description This is a base class for widgets that use the 'controls' module.
#' @include widget.R
#' @export
CoreWidgetClass <- R6Class_("CoreWidget",
  inherit = WidgetClass,
  public = list(
    #' @field _model_module Name of the Javascript module with the model
    `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _model_module_version Version of the module where the model is defined
    `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version()),sync=TRUE),
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("DOMWidgetModel"),sync=TRUE),
    #' @field _view_module Version of the module where the view is defined
    `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _view_module_version Version of the module where the view is defined
    `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version()),sync=TRUE)
  )
)
