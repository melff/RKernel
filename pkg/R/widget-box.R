#' @include widget.R widget-description.R

#' @export
BoxClass <- R6Class_("Box",
   inherit = DOMWidgetClass,
   public = list(
    `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
    `_model_name` = structure(Unicode("BoxModel"),sync=TRUE),
    `_view_name` = structure(Unicode("BoxView"),sync=TRUE),
    `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
    children = structure(Vector(),sync=TRUE),
    box_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default="")),
    initialize = function(children=list(),...){
        super$initialize(...)
        if(length(children)==1 && is.list(children[[1]]))
            children <- children[[1]]
        self$children <- children
        #self$send_state()
        self$on_displayed(self$notify_children_displayed)
    },
    notify_children_displayed = function(){
        for(child in self$children)
            child$handle_displayed()
    }
   )
)

ContainerClass_new <- function(Class,...){
    args <- list(...)
    argnames <- names(args)
    if(!length(argnames)){
        children <- args
        call_args <- list(children=children)
    }
    else{
        named_args <- nzchar(argnames)
        if(!("children" %in% argnames)){
            children <- args[!named_args]
            other <- args[named_args]
            call_args <- c(list(children=children),
                           other)
        }
        else
            call_args <- args
    }
    do.call(Class$new,call_args)
}

#' @export
Box <- function(...) ContainerClass_new(Class=BoxClass,...)

#' @export
HBoxClass <- R6Class_("HBox",
    inherit = BoxClass,
    public=list(
        `_model_name` = structure(Unicode("HBoxModel"),sync=TRUE),
        `_view_name` = structure(Unicode("HBoxView"),sync=TRUE)
    )
)

#' @export
HBox <- function(...) ContainerClass_new(Class=HBoxClass,...)

#' @export
VBoxClass <- R6Class_("VBox",
    inherit = BoxClass,
    public=list(
        `_model_name` = structure(Unicode("VBoxModel"),sync=TRUE),
        `_view_name` = structure(Unicode("VBoxView"),sync=TRUE)
    )
)

#' @export
VBox <- function(...) ContainerClass_new(Class=VBoxClass,...)


#' @export
GridBoxClass <- R6Class_("GridBox",
    inherit = BoxClass,
    public=list(
        `_model_name` = structure(Unicode("GridBoxModel"),sync=TRUE),
        `_view_name` = structure(Unicode("GridBoxView"),sync=TRUE)
    )
)

#' @export
GridBox <- function(...) ContainerClass_new(Class=GridBoxClass,...)
