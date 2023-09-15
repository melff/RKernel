#' Color picker widgets
#'
#' @description  A class and constructor function to create color picker widgets
#'
#' @include widget-value.R
#' @name ColorPicker
NULL

#' @rdname ColorPicker 
#' @export
ColorPickerClass <- R6Class_("ColorPicker",
   inherit = ValueWidgetClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("ColorPickerModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("ColorPickerView"),sync=TRUE),
    #' @field concise Boolean, whether the a short version should be shown
    concise = structure(Boolean(FALSE),sync=TRUE),
    #' @field disabled Boolean, whether the button is disabled
    disabled = structure(Boolean(FALSE),sync=TRUE),
    #' @field value Unicode string, the color value
    value = structure(Color("black"),sync=TRUE)
   )
)

#' @describeIn ColorPicker The constructor for color-picker widgets
#' @param ... Arguments passed to the inializer
#' @export
ColorPicker <- function(...) ColorPickerClass$new(...)
