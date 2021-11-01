#' @include widget.R widget-description.R

#' @export
SliderStyleClass <- R6Class_("SliderStyle",
  inherit = DescriptionStyleClass,
  public = list(
    `_model_name` = structure(Unicode("SliderStyleModel"),sync=TRUE),
    handle_color = structure(Unicode(character(0)),sync=TRUE)
  )
)

#' @export
IntSliderClass <- R6Class_("IntSlider",
   inherit = DescriptionWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("IntSliderModel"),sync=TRUE),
    `_view_name` = structure(Unicode("IntSliderView"),sync=TRUE),
    step = structure(Integer(1L),sync=TRUE),
    orientation = structure(StrEnum(c("horizontal","vertical"),initial="horizontal"),sync=TRUE),
    readout = structure(Boolean(TRUE),sync=TRUE),
    readout_format = structure(Unicode("d"),sync=TRUE),
    continuous_update = structure(Boolean(TRUE),sync=TRUE),
    disabled = structure(Boolean(FALSE),sync=TRUE),
    style = structure(R6Instance(SliderStyleClass),sync=TRUE),
    value = structure(BoundedInteger(0L,range=c(0L,100L)),sync=TRUE),
    min = structure(Integer(0L),sync=TRUE),
    max = structure(Integer(100L),sync=TRUE),
    description_tooltip = structure(Unicode(character(0)),sync=TRUE)
   )
)

#' @export
IntSlider <- function(...) IntSliderClass$new(...)
