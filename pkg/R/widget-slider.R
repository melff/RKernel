#' Sliders
#'
#' @description Classes and constructor functions for sliders (integer and floating-point ones)
#' 
#' @include widget-integer.R widget-float.R
#' @name Slider
NULL

#' @rdname Slider
#' @export
SliderStyleClass <- R6Class_("SliderStyle",
  inherit = DescriptionStyleClass,
  public = list(
      #' @field _model_name Name of the Javascript frontend model
      `_model_name` = structure(Unicode("SliderStyleModel"),sync=TRUE),
      #' @field handle_color Unicode string, the color of the slider handle
      handle_color = structure(Unicode(character(0)),sync=TRUE)
  ))

#' @rdname Slider
#' @export
IntSliderClass <- R6Class_("IntSlider",
   inherit = BoundedIntWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("IntSliderModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("IntSliderView"),sync=TRUE),
       #' @field step An \code{\link{Integer}} traitlet, the minimal step size per slider movement
       step = structure(Integer(1L),sync=TRUE),
       #' @field orientation A Unicode string, either "horizontal" or "vertical"
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       #' @field readout A logical value, whether the value should be showns (read out)
       readout = structure(Boolean(TRUE),sync=TRUE),
       #' @field readout_format A unicode string, the format specification
       readout_format = structure(Unicode("d"),sync=TRUE),
       #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
       continuous_update = structure(Boolean(TRUE),sync=TRUE),
       #' @field disabled A logical value, whether the slider is disabled
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field style A SliderStyle widget
       style = structure(R6Instance(SliderStyleClass),sync=TRUE),
       #' @field behavior A string that describes the ddragging behavior.
       behavior = structure(StrEnum(c("drag-tap","drag-snap","tap","drag","snap")))
   ))

#' @describeIn Slider An integer slider constructor
#' @param value An integer, the current value of the slider
#' @param min An integer, the minimum value
#' @param max An integer, the maximum value
#' @param ... Other arguments.
#' @export
IntSlider <- function(value=0L,min=0L,max=100L,...) 
    IntSliderClass$new(value=value,
                       min=min,
                       max=max,
                       ...)



#' @rdname Slider
#' @export
IntRangeSliderClass <- R6Class_("IntRangeSlider",
   inherit = BoundedIntRangeWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("IntRangeSliderModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("IntRangeSliderView"),sync=TRUE),
       #' @field step An \code{\link{Integer}} traitlet, the minimal step size per slider movement
       step = structure(Integer(1L),sync=TRUE),
       #' @field orientation A Unicode string, either "horizontal" or "vertical"
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       #' @field readout A logical value, whether the value should be showns (read out)
       readout = structure(Boolean(TRUE),sync=TRUE),
       #' @field readout_format A logical value, whether values should be updated as the slider is moved by the user
       readout_format = structure(Unicode("d"),sync=TRUE),
       #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
       continuous_update = structure(Boolean(TRUE),sync=TRUE),
       #' @field disabled A logical value, whether the slider is disabled
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field style A SliderStyle widget
       style = structure(R6Instance(SliderStyleClass),sync=TRUE),
       #' @field behavior A string that describes the ddragging behavior.
       behavior = structure(StrEnum(c("drag-tap","drag-snap","tap","drag","snap")))
   ))

#' @describeIn Slider An integer range slider constructor
#' @param value A pair integers, the current value of the range slider
#' @param min An integer, the minimum value
#' @param max An integer, the maximum value
#' @param ... Other arguments.
#' @export
IntRangeSlider <- function(value=c(0L,50L),min=0L,max=100L,...) 
    IntRangeSliderClass$new(value=value,
                       min=min,
                       max=max,
                       ...)




#' @rdname Slider
#' @export
FloatSliderClass <- R6Class_("FloatSlider",
   inherit = BoundedFloatWidgetClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("FloatSliderModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript view in the frontend
    `_view_name` = structure(Unicode("FloatSliderView"),sync=TRUE),
    #' @field step A \code{\link{Float}} traitlet, the minimal step size per slider movement
    step = structure(Float(0.01),sync=TRUE),
    #' @field orientation A Unicode string, either "horizontal" or "vertical"
    orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
    #' @field readout A logical value, whether the value should be showns (read out)
    readout = structure(Boolean(TRUE),sync=TRUE),
    #' @field readout_format A logical value, whether values should be updated as the slider is moved by the user
    readout_format = structure(Unicode(".2f"),sync=TRUE),
    #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
    continuous_update = structure(Boolean(TRUE),sync=TRUE),
    #' @field disabled A logical value, whether the slider is disabled
    disabled = structure(Boolean(FALSE),sync=TRUE),
    #' @field style A SliderStyle widget
    style = structure(R6Instance(SliderStyleClass),sync=TRUE),
    #' @field behavior A string that describes the ddragging behavior.
    behavior = structure(StrEnum(c("drag-tap","drag-snap","tap","drag","snap")))
))

#' @describeIn Slider A floating point slider constructor
#' @param value A floating point number, the current value of the slider
#' @param min A floating point number, the minimum value
#' @param max A floating point number, the maximum value
#' @param ... Other arguments.
#' @export
FloatSlider <- function(value=0,min=0,max=100,...) 
    FloatSliderClass$new(value=value,
                         min=min,
                         max=max,
                         ...)

#' @rdname Slider
#' @export
FloatRangeSliderClass <- R6Class_("FloatRangeSlider",
   inherit = BoundedFloatRangeWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("FloatRangeSliderModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("FloatRangeSliderView"),sync=TRUE),
       #' @field step A \code{\link{Float}} traitlet, the minimal step size per slider movement
       step = structure(Float(0.01),sync=TRUE),
       #' @field orientation A Unicode string, either "horizontal" or "vertical"
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       #' @field readout A logical value, whether the value should be showns (read out)
       readout = structure(Boolean(TRUE),sync=TRUE),
       #' @field readout_format A logical value, whether values should be updated as the slider is moved by the user
       readout_format = structure(Unicode(".2f"),sync=TRUE),
       #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
       continuous_update = structure(Boolean(TRUE),sync=TRUE),
       #' @field disabled A logical value, whether the slider is disabled
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field style A SliderStyle widget
       style = structure(R6Instance(SliderStyleClass),sync=TRUE),
       #' @field behavior A string that describes the ddragging behavior.
       behavior = structure(StrEnum(c("drag-tap","drag-snap","tap","drag","snap")))
   ))

#' @describeIn Slider A floating point slider range constructor
#' @param value A pair of floating point numbers, the current value of the range slider
#' @param min A floating point number, the minimum value
#' @param max A floating point number, the maximum value
#' @param ... Other arguments.
#' @export
FloatRangeSlider <- function(value=c(0,50),min=0,max=100,...) 
    FloatRangeSliderClass$new(value=value,
                       min=min,
                       max=max,
                       ...)


#' @rdname Slider
#' @export
FloatLogSliderClass <- R6Class_("FloatLogSlider",
   inherit = BoundedLogFloatWidgetClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("FloatLogSliderModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript view in the frontend
    `_view_name` = structure(Unicode("FloatLogSliderView"),sync=TRUE),
    #' @field step A \code{\link{Float}} traitlet, the minimal step size per slider movement
    step = structure(Float(0.01),sync=TRUE),
    #' @field orientation A Unicode string, either "horizontal" or "vertical"
    orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
    #' @field readout A logical value, whether the value should be showns (read out)
    readout = structure(Boolean(TRUE),sync=TRUE),
    #' @field readout_format A logical value, whether values should be updated as the slider is moved by the user
    readout_format = structure(Unicode(".3g"),sync=TRUE),
    #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
    continuous_update = structure(Boolean(TRUE),sync=TRUE),
    #' @field disabled A \code{\link{Boolean}} traitlet, whether the slider is disabled
    disabled = structure(Boolean(FALSE),sync=TRUE),
    #' @field base A \code{\link{Float}} traitlet, the base of the logarithm
    base = structure(Float(10.0),sync=TRUE),
    #' @field style A SliderStyle widget
    style = structure(R6Instance(SliderStyleClass),sync=TRUE),
    #' @field behavior A string that describes the ddragging behavior.
    behavior = structure(StrEnum(c("drag-tap","drag-snap","tap","drag","snap")))
   ))

#' @describeIn Slider A floating point log-slider constructor
#' @param value A floating point number, the current value of the slider
#' @param min A floating point number, the minimum value
#' @param max A floating point number, the maximum value
#' @param base A floating point number, the base of the logarithm
#' @param ... Other arguments.
#' @export
FloatLogSlider <- function(value=1,min=0,max=40,base=10,...) 
    FloatLogSliderClass$new(value=value,
                            min=min,
                            max=max,
                            base=base,
                            ...)
