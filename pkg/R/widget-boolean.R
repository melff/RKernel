#' Checkbox Widgets
#'
#' @description A class and a constructor function to create checkbox widgets
#' 
#' @include widget-value.R
#' @name Checkboxes
NULL

#' @rdname Checkboxes 
#' @export
CheckboxClass <- R6Class_("Checkbox",
   inherit = ValueWidgetClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("CheckboxModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("CheckboxView"),sync=TRUE),
    #' @field indent Boolean, whether to indent the checkbox
    indent = structure(Boolean(TRUE),sync=TRUE),
    #' @field value Boolean, whether the box is checked
    value = structure(Boolean(FALSE),sync=TRUE)
   )
)

#' @describeIn Checkboxes A checkbox constructor
#' @export
Checkbox <- function(...) CheckboxClass$new(...)

#' Tobble-Button Widgets
#'
#' @description A constructor function and a class to create toggle-button widgets
#' 
#' @include widget-value.R
#' @name Togglebuttons
NULL


#' @rdname Togglebuttons
#' @export
ToggleButtonClass <- R6Class_("ToggleButton",
   inherit = ValueWidgetClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("ToggleButtonModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("ToggleButtonView"),sync=TRUE),
    #' @field value Boolean, whether the box is checked
    value = structure(Boolean(FALSE),sync=TRUE),
    #' @field tooltip A tooltip
    tooltip = structure(Unicode(),sync=TRUE),
    #' @field icon An icon (a fontawesome icon name)
    icon = structure(Unicode(""),sync=TRUE),
    #' @field button_style The string that describes the button style
    button_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default=""))
   )
)

#' @describeIn Togglebuttons A toggle-button constructor
#' @export
ToggleButton <- function(...) ToggleButtonClass$new(...)

#' Validity Indicator Widgets
#'
#' @description A constructor function and a class to create toggle-button widgets
#' 
#' @include widget-value.R
#' @name Valid
NULL

#' @rdname Valid
#' @export
ValidClass <- R6Class_("Valid",
   inherit = ValueWidgetClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("ValidModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("ValidView"),sync=TRUE),
    #' @field indent Boolean, whether to indent the indicator widget
    indent = structure(Boolean(TRUE),sync=TRUE),
    #' @field value Boolean, whether the validity should be indicated
    value = structure(Boolean(FALSE),sync=TRUE),
    #' @field readout Text to be shown if the Widget value is FALSE
    readout = structure(Unicode("Invalid"),sync=TRUE)
   )
)

#' @describeIn Valid A constructor for validity indicator widgets
#' @export
Valid <- function(...) ValidClass$new(...)
