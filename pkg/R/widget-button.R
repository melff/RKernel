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
    click_handlers = list(),
    initialize = function(...){
        super$initialize(...)
        self$click_handlers <- CallbackDispatcher()
        self$on_msg(self$handle_button_msg)
    },
    on_click = function(handler,remove=FALSE){
        self$click_handlers$register(handler,remove)
    },
    click = function(){
        self$click_handlers$run()
    },
    handle_button_msg = function(self,content){
        if("event" %in% names(content) && content$event == "click"){
            self$click()
        }
    }
   )
)

#' @export
Button <- function(...) ButtonClass$new(...)
