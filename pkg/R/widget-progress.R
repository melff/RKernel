#' Progress bars
#'
#' @description Classes and constructor functions for progress bars and styling of them
#' 
#' @include widget-integer.R widget-float.R
#' @name Progress
NULL

#' @rdname Progress
#' @export
ProgressStyleClass <- R6Class_("ProgrssStyle",
  inherit = DescriptionStyleClass,
  public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("ProgressStyleModel"),sync=TRUE),
    #' @field bar_color The colour of the progress bar
    bar_color = structure(Unicode(character(0)),sync=TRUE)
  ))


#' @describeIn Progress A constructor for a progress bar style
#' @param ... Arguments passed to the inializer
#' @export
ProgressStyle <- function(...) ProgressStyleClass$new(...)

#' @rdname Progress
#' @export
IntProgressClass <- R6Class_("IntProgress",
   inherit = BoundedIntWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("IntProgressModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("ProgressView"),sync=TRUE),
       #' @field orientation Orientation of the progress bar, either "horizontal" or "vertical"
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       #' @field bar_style General style of the progress bar, either "successs", "info", "warning" or "danger"
       bar_style = structure(StrEnum(c("success","info","warning","danger",""),default=""),sync=TRUE),
       #' @field style Styling of the progress bar, an instance of "ProgressStyleClass"
       style = structure(R6Instance(ProgressStyleClass),sync=TRUE)
   ))

#' @describeIn Progress A constructor for a progress bar style
#' @param value An integer, the initial position of the progress bar
#' @param min An integer, the minumum value
#' @param max An integer, the maximum value
#' @param ... Other arguments.
#' @export
IntProgress <- function(value=0L,min=0L,max=100L,...) 
    IntProgressClass$new(value=value,
                       min=min,
                       max=max,
                       ...)

#' @rdname Progress
#' @export
FloatProgressClass <- R6Class_("FloatProgress",
   inherit = BoundedFloatWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("FloatProgressModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("ProgressView"),sync=TRUE),
       #' @field orientation Orientation of the progress bar, either "horizontal" or "vertical"
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       #' @field bar_style General style of the progress bar, either "successs", "info", "warning" or "danger"
       bar_style = structure(StrEnum(c("success","info","warning","danger",""),default=""),sync=TRUE),
       #' @field style Styling of the progress bar, an instance of "ProgressStyleClass"
       style = structure(R6Instance(ProgressStyleClass),sync=TRUE)
   ))

#' @describeIn Progress A constructor for a progress bar style
#' @param value A floating point number, the initial position of the progress bar
#' @param min An floating point number, the minumum value
#' @param max An floating point number, the maximum value
#' @param ... Other arguments.
#' @export
FloatProgress <- function(value=0L,min=0L,max=100L,...) 
    FloatProgressClass$new(value=value,
                       min=min,
                       max=max,
                       ...)
