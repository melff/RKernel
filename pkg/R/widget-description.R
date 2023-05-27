#' Styling of Decscription Widgets
#'
#' @description Objects of this class contain the CSS styling of description widgets
#' 
#' @include widget.R widget-dom.R
#' @name DescriptionStyle
NULL

#' @rdname DescriptionStyle
#' @export
DescriptionStyleClass <- R6Class_("DescriptionStyle",
  inherit = WidgetClass,
  public = list(
    #' @field _model_module Name of the Javascript module with the model
    `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _model_module_version Version of the module where the model is defined
    `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("DescriptionStyleModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("StyleView"),sync=TRUE),
    #' @field _view_module Version of the module where the view is defined
    `_view_module` = structure(Unicode("@jupyter-widgets/base"),sync=TRUE),
    #' @field _view_module_version Version of the module where the view is defined
    `_view_module_version` = structure(Unicode(jupyter_widgets_base_version),sync=TRUE),
    #' @field description_width Width of the description
    description_width = structure(Unicode(character(0)),sync=TRUE)
  )
)

#' @describeIn DescriptionStyle A Constructor Function for "DescriptionStyle" objects
#' @param ... Arguments passed to the inializer
#' @export
DescriptionStyle <- function(...) DescriptionStyleClass$new(...)

#' Decscription Widgets
#'
#' @description Objects of this class have an optional description and a description tooltip field
#' 
#' @include widget.R widget-dom.R
#' @name DescriptionWidget
NULL

#' @rdname DescriptionWidget
#' @export
DescriptionWidgetClass <- R6Class_("DescriptionWidget",
  inherit = DOMWidgetClass,
  public = list(
    #' @field _model_module Name of the Javascript module with the model
    `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _model_module_version Version of the module where the model is defined
    `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
    #' @field _view_module Version of the module where the view is defined
    `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _view_module_version Version of the module where the view is defined
    `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("DescriptionModel"),sync=TRUE),
    #' @field description An optional description string
    description = structure(Unicode(""),sync=TRUE),
    #' @field description_tooltip An optional description tooltip
    description_tooltip = structure(Unicode(""),sync=TRUE),
    #' @field style A "DescriptionStyle" object
    style = structure(R6Instance(DescriptionStyleClass),sync=TRUE)
  )
)

#' @describeIn DescriptionWidget A Constructor Function for "DescriptionWidget" objects
#' @param ... Arguments passed to the inializer
#' @export
DescriptionWidget <- function(...) DescriptionWidgetClass$new(...)
