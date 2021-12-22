#' @include widget-integer.R widget-float.R

#' @export
SliderStyleClass <- R6Class_("SliderStyle",
  inherit = DescriptionStyleClass,
  public = list(
    `_model_name` = structure(Unicode("SliderStyleModel"),sync=TRUE),
    handle_color = structure(Unicode(character(0)),sync=TRUE)
  ))

#' @export
IntSliderClass <- R6Class_("IntSlider",
   inherit = BoundedIntegerWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("IntSliderModel"),sync=TRUE),
       `_view_name` = structure(Unicode("IntSliderView"),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE),
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       readout = structure(Boolean(TRUE),sync=TRUE),
       readout_format = structure(Unicode("d"),sync=TRUE),
       continuous_update = structure(Boolean(TRUE),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       style = structure(R6Instance(SliderStyleClass),sync=TRUE)
   ))

#' @export
IntSlider <- function(value=0L,min=0L,max=100L,...) 
    IntSliderClass$new(value=value,
                       min=min,
                       max=max,
                       ...)



#' @export
IntRangeSliderClass <- R6Class_("IntRangeSlider",
   inherit = BoundedIntRangeWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("IntRangeSliderModel"),sync=TRUE),
       `_view_name` = structure(Unicode("IntRangeSliderView"),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE),
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       readout = structure(Boolean(TRUE),sync=TRUE),
       readout_format = structure(Unicode("d"),sync=TRUE),
       continuous_update = structure(Boolean(TRUE),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       style = structure(R6Instance(SliderStyleClass),sync=TRUE)
   ))

#' @export
IntRangeSlider <- function(value=c(0L,50L),min=0L,max=100L,...) 
    IntRangeSliderClass$new(value=value,
                       min=min,
                       max=max,
                       ...)




#' @export
FloatSliderClass <- R6Class_("FloatSlider",
   inherit = BoundedFloatWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("FloatSliderModel"),sync=TRUE),
    `_view_name` = structure(Unicode("FloatSliderView"),sync=TRUE),
    step = structure(Float(0.01),sync=TRUE),
    orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
    readout = structure(Boolean(TRUE),sync=TRUE),
    readout_format = structure(Unicode(".2f"),sync=TRUE),
    continuous_update = structure(Boolean(TRUE),sync=TRUE),
    disabled = structure(Boolean(FALSE),sync=TRUE),
    style = structure(R6Instance(SliderStyleClass),sync=TRUE)
   ))

#' @export
FloatSlider <- function(value=0,min=0,max=100,...) 
    FloatSliderClass$new(value=value,
                         min=min,
                         max=max,
                         ...)

#' @export
FloatRangeSliderClass <- R6Class_("FloatRangeSlider",
   inherit = BoundedFloatRangeWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("FloatRangeSliderModel"),sync=TRUE),
       `_view_name` = structure(Unicode("FloatRangeSliderView"),sync=TRUE),
       step = structure(Float(0.01),sync=TRUE),
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       readout = structure(Boolean(TRUE),sync=TRUE),
       readout_format = structure(Unicode(".2f"),sync=TRUE),
       continuous_update = structure(Boolean(TRUE),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       style = structure(R6Instance(SliderStyleClass),sync=TRUE)
   ))

#' @export
FloatRangeSlider <- function(value=c(0,50),min=0,max=100,...) 
    FloatRangeSliderClass$new(value=value,
                       min=min,
                       max=max,
                       ...)


#' @export
FloatLogSliderClass <- R6Class_("FloatLogSlider",
   inherit = BoundedLogFloatWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("FloatLogSliderModel"),sync=TRUE),
    `_view_name` = structure(Unicode("FloatLogSliderView"),sync=TRUE),
    step = structure(Float(0.01),sync=TRUE),
    orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
    readout = structure(Boolean(TRUE),sync=TRUE),
    readout_format = structure(Unicode(".3g"),sync=TRUE),
    continuous_update = structure(Boolean(TRUE),sync=TRUE),
    disabled = structure(Boolean(FALSE),sync=TRUE),
    base = structure(Float(10.0),sync=TRUE),
    style = structure(R6Instance(SliderStyleClass),sync=TRUE)
   ))

#' @export
FloatLogSlider <- function(value=1,min=0,max=40,base=10,...) 
    FloatLogSliderClass$new(value=value,
                            min=min,
                            max=max,
                            base=base,
                            ...)
