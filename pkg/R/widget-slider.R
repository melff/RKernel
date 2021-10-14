IntSliderClass <- R6Class("IntSlider",
   inherit = DescriptionWidgetClass,
   public = list(
    `_model_name` = trait("IntSliderView",sync=TRUE),
    `_view_name` = trait("IntSliderModel",sync=TRUE),
    step = trait(1L,sync=TRUE),
    orientation = trait(c("horizontal","vertical"),type="str_enum",default="horizontal",sync=TRUE),
    readout = trait(TRUE,sync=TRUE),
    readout_format = trait("d",sync=TRUE),
    continuous_update = trait(TRUE,sync=TRUE),
    disabled = trait(FALSE,sync=TRUE),
    style = trait(quote(SliderStyle()),sink=TRUE)
   ),
   active = list(
       value = BoundedInteger()
   )
)
