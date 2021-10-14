IntSliderClass <- R6Class("IntSlider",
   inherit = DescriptionWidgetClass,
   public = list(
    `_model_name` = Unicode("IntSliderView",sync=TRUE),
    `_view_name` = Unicode("IntSliderModel",sync=TRUE),
    step = Integer(1L,sync=TRUE),
    orientation = StrEnum(c("horizontal","vertical"),initial="horizontal",sync=TRUE),
    readout = Boolean(TRUE,sync=TRUE),
    readout_format = Unicode("d",sync=TRUE),
    continuous_update = Boolean(TRUE,sync=TRUE),
    disabled = Boolean(FALSE,sync=TRUE),
    style = Instance(SliderStyleClass,sync=TRUE),
    value = BoundedInteger(0L,range=c(0L,100L),sync=TRUE)
   )
)

#' @export
SliderStyleClass <- R6Class_("SliderStyle",
  inherit = DescriptionStyleClass,
  public = list(
    `_model_name` = Unicode("StyleStyleModel",sync=TRUE),
    handle_color = Unicode(character(0),sync=TRUE)
  )
)
