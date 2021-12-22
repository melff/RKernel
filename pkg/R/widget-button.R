#' @include widget.R widget-description.R

#' @export
ButtonStyleClass <- R6Class_("ButtonStyle",
  inherit = DescriptionStyleClass,
  public = list(
    `_model_name` = structure(Unicode("ButtonStyleModel"),sync=TRUE),
    button_color = structure(Unicode(character(0)),sync=TRUE),
    font_family = structure(Unicode(character(0)),sync=TRUE),
    font_size = structure(Unicode(character(0)),sync=TRUE),
    font_style = structure(Unicode(character(0)),sync=TRUE),
    font_variant = structure(Unicode(character(0)),sync=TRUE),
    font_weight = structure(Unicode(character(0)),sync=TRUE),
    text_color = structure(Unicode(character(0)),sync=TRUE),
    text_decoration = structure(Unicode(character(0)),sync=TRUE)
  )
)

#' @export
ButtonStyle <- function(...) ButtonStyleClass$new(...)

#' @export
ButtonClass <- R6Class_("Button",
   inherit = DOMWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("ButtonModel"),sync=TRUE),
    `_view_name` = structure(Unicode("ButtonView"),sync=TRUE),
    description = structure(Unicode(""),sync=TRUE),
    disabled = structure(Boolean(FALSE),sync=TRUE),
    icon = structure(Unicode(""),sync=TRUE),
    button_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default="")),
    tooltip = structure(Unicode(""),sync=TRUE),
    style = structure(R6Instance(ButtonStyleClass),sync=TRUE),
    on_click = function(handler,remove=FALSE){
        self$on_event("click",handler,remove)
    },
    click = function(){
        self$handle_event("click",args=NULL)
    }
   )
)

#' @export
Button <- function(...) ButtonClass$new(...)
