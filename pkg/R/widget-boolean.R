#' @include widget-value.R

#' @export
CheckboxClass <- R6Class_("Checkbox",
   inherit = ValueWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("CheckboxModel"),sync=TRUE),
    `_view_name` = structure(Unicode("CheckboxView"),sync=TRUE),
    indent = structure(Boolean(TRUE),sync=TRUE),
    value = structure(Boolean(FALSE),sync=TRUE)
   )
)

#' @export
Checkbox <- function(...) CheckboxClass$new(...)


#' @export
ToggleButtonClass <- R6Class_("ToggleButton",
   inherit = ValueWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("ToggleButtonModel"),sync=TRUE),
    `_view_name` = structure(Unicode("ToggleButtonView"),sync=TRUE),
    value = structure(Boolean(FALSE),sync=TRUE),
    tooltip = structure(Unicode(),sync=TRUE),
    icon = structure(Unicode(""),sync=TRUE),
    button_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default=""))
   )
)

#' @export
ToggleButton <- function(...) ToggleButtonClass$new(...)

#' @export
ValidClass <- R6Class_("Valid",
   inherit = ValueWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("ValidModel"),sync=TRUE),
    `_view_name` = structure(Unicode("ValidView"),sync=TRUE),
    indent = structure(Boolean(TRUE),sync=TRUE),
    value = structure(Boolean(FALSE),sync=TRUE),
    readout = structure(Unicode("Invalid"),sync=TRUE)
   )
)

#' @export
Valid <- function(...) ValidClass$new(...)
