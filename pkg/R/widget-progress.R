#' @include widget-integer.R widget-float.R

#' @export
ProgressStyleClass <- R6Class_("ProgrssStyle",
  inherit = DescriptionStyleClass,
  public = list(
    `_model_name` = structure(Unicode("ProgressStyleModel"),sync=TRUE),
    bar_color = structure(Unicode(character(0)),sync=TRUE)
  ))

#' @export
IntProgressClass <- R6Class_("IntProgress",
   inherit = BoundedIntWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("IntProgressModel"),sync=TRUE),
       `_view_name` = structure(Unicode("ProgressView"),sync=TRUE),
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       bar_style = structure(StrEnum(c("success","info","warning","danger",""),default=""),sync=TRUE),
       style = structure(R6Instance(ProgressStyleClass),sync=TRUE)
   ))

#' @export
IntProgress <- function(value=0L,min=0L,max=100L,...) 
    IntProgressClass$new(value=value,
                       min=min,
                       max=max,
                       ...)

#' @export
FloatProgressClass <- R6Class_("FloatProgress",
   inherit = BoundedFloatWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("FloatProgressModel"),sync=TRUE),
       `_view_name` = structure(Unicode("ProgressView"),sync=TRUE),
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       bar_style = structure(StrEnum(c("success","info","warning","danger",""),default=""),sync=TRUE),
       style = structure(R6Instance(ProgressStyleClass),sync=TRUE)
   ))

#' @export
FloatProgress <- function(value=0L,min=0L,max=100L,...) 
    FloatProgressClass$new(value=value,
                       min=min,
                       max=max,
                       ...)
