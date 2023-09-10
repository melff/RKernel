#' Checkbox Widgets
#'
#' @description A class and a constructor function to create checkbox widgets
#' 
#' @include widget-value.R
#' @name Checkboxes
NULL

#' @rdname Checkboxes
#' @export
CheckboxStyleClass <- R6Class_("CheckboxStyle",
   inherit = DescriptionStyleClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("CheckboxStyleModel"),sync=TRUE),
    #' @field background The background color
    background = structure(Unicode(""),sync=TRUE),
    #' @field required_version Minimum required ipywidgets version in which the
    #'        current widget class is supported.
    required_version = c(8,0,0)
   )
)

#' @describeIn Checkboxes The constructor for checkbox widgets
#' @param ... Arguments passed to the inializer
#' @export
CheckboxStyle <- function(...) CheckboxStyleClass$new(...)

#' @rdname Checkboxes 
#' @export
CheckboxClass <- R6Class_("Checkbox",
   inherit = ValueWidgetClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("CheckboxModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("CheckboxView"),sync=TRUE),
    #' @field description A button description
    description = structure(Unicode(""),sync=TRUE),
    #' @field disabled Boolean, whether the button is disabled
    disabled = structure(Boolean(FALSE),sync=TRUE),
    #' @field indent Boolean, whether to indent the checkbox
    indent = structure(Boolean(TRUE),sync=TRUE),
    #' @field value Boolean, whether the box is checked
    value = structure(Boolean(FALSE),sync=TRUE),
    #' @field style The checkbox style, an object of class "CheckboxStyleClass"
    style = structure(R6Instance(CheckboxStyleClass),sync=TRUE)
   )
)

#' @describeIn Checkboxes The constructor for checkbox styles
#' @param ... Arguments passed to the inializer
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
ToggleButtonStyleClass <- R6Class_("ToggleButtonStyle",
   inherit = DescriptionStyleClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("ToggleButtonStyleModel"),sync=TRUE),
    #' @field font_family The font family
    font_family = structure(Unicode(""),sync=TRUE),
    #' @field font_size The font size
    font_size = structure(Unicode(""),sync=TRUE),
    #' @field font_style The font style
    font_style = structure(Unicode(""),sync=TRUE),
    #' @field font_variant The font variant
    font_variant = structure(Unicode(""),sync=TRUE),
    #' @field font_weight The font weight
    font_weight = structure(Unicode(""),sync=TRUE),
    #' @field text_color The text color
    text_color = structure(Unicode(""),sync=TRUE),
    #' @field text_decoration The text decoration
    text_decoration = structure(Unicode(""),sync=TRUE),
    #' @field required_version Minimum required ipywidgets version in which the
    #'        current widget class is supported.
    required_version = c(8,0,0)
   )
)

#' @describeIn Togglebuttons The constructor for Togglebuttons styles
#' @param ... Arguments passed to the inializer
#' @export
ToggleButtonStyle <- function(...) ToggleButtonStyleClass$new(...)


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
    #' @field description A button description
    description = structure(Unicode(""),sync=TRUE),
    #' @field disabled Boolean, whether the button is disabled
    disabled = structure(Boolean(FALSE),sync=TRUE),
    #' @field icon An icon (a fontawesome icon name)
    icon = structure(Unicode(""),sync=TRUE),
    #' @field button_style The string that describes the button style
    button_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default="")),
    #' @field style The toggle button style, an object of class "ToggleButtonStyleClass"
    style = structure(R6Instance(ToggleButtonStyleClass),sync=TRUE)
   )
)

#' @describeIn Togglebuttons A toggle-button constructor
#' @param ... Arguments passed to the inializer
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
#' @param ... Arguments passed to the inializer
#' @export
Valid <- function(...) ValidClass$new(...)
