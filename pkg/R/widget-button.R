#' Buttons
#'
#' @description Classes and constructor functions for boxes and box styles
#' 
#' @include widget.R widget-description.R
#' @name Buttons
NULL

#' @rdname Buttons
#' @order 4
#' @export
ButtonStyleClass <- R6Class_("ButtonStyle",
  inherit = DescriptionStyleClass,
  public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("ButtonStyleModel"),sync=TRUE),
    #' @field button_color The colour of the button
    button_color = structure(Unicode(character(0)),sync=TRUE),
    #' @field font_family The font family of the button label
    font_family = structure(Unicode(character(0)),sync=TRUE),
    #' @field font_size The font size of the button label
    font_size = structure(Unicode(character(0)),sync=TRUE),
    #' @field font_style The font style of the button label
    font_style = structure(Unicode(character(0)),sync=TRUE),
    #' @field font_variant The font variant of the button label
    font_variant = structure(Unicode(character(0)),sync=TRUE),
    #' @field font_weight The font weight of the button label
    font_weight = structure(Unicode(character(0)),sync=TRUE),
    #' @field text_color The text colour of the button label
    text_color = structure(Unicode(character(0)),sync=TRUE),
    #' @field text_decoration The text decoration of the button label
    text_decoration = structure(Unicode(character(0)),sync=TRUE)
  )
)

#' @describeIn Buttons A constructor for a button style
#' @order 3
#' @param ... Arguments passed to the inializer
#' @export
ButtonStyle <- function(...) ButtonStyleClass$new(...)


#' @rdname Buttons
#' @order 2
#' @export
ButtonClass <- R6Class_("Button",
   inherit = DOMWidgetClass,
   public = list(
    #' @field _model_module Name of the Javascript module with the model
    `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _model_module_version Version of the module where the model is defined
    `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version()),sync=TRUE),
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("ButtonModel"),sync=TRUE),
    #' @field _view_module Name of the module where the view is defined
    `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _view_module_version Version of the module where the view is defined
    `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version()),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("ButtonView"),sync=TRUE),
    #' @field description A button description
    description = structure(Unicode(""),sync=TRUE),
    #' @field disabled Boolean, whether the button is disabled
    disabled = structure(Boolean(FALSE),sync=TRUE),
    #' @field icon Name of an optional icon
    icon = structure(Unicode(""),sync=TRUE),
    #' @field button_style The string that describes the button style
    button_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default=""),sync=TRUE),
    #' @field tooltip An optional tooltip string
    tooltip = structure(Unicode(""),sync=TRUE),
    #' @field style The button style, an object of class "ButtonStyleClass"
    style = structure(R6Instance(ButtonStyleClass),sync=TRUE),
    #' @description Add or remove a click handler
    #' @param handler A function that is called when the button is clicked
    #' @param remove Logical value, whether the handler is to be removed
    on_click = function(handler,remove=FALSE){
        self$on_event("click",handler,remove)
    },
    #' @description Function that calls the click event handlers
    click = function(){
        self$handle_event("click",args=NULL)
    }
   )
)

#' @describeIn Buttons A button constructor
#' @param ... Arguments passed to the inializer
#' @order 1
#' @export
Button <- function(...) ButtonClass$new(...)
