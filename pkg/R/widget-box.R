#' Box Containers
#'
#' @description Classes and constructor functions to create box container widgets
#'
#' @details \code{VBox} creates vertical boxes, \code{HBox} creates horizontal boxes,
#'     \code{GridBox} creates a grid box. In a vertical box widgets are arranged one below the other,
#'     in a horizontal box widgets are arranged side-by-side, in a grid box widget are arraned in a
#'     grid.
#' 
#' @include widget.R widget-description.R
#' @name Boxes

#' @rdname Boxes
#' @export
BoxClass <- R6Class_("Box",
   inherit = DOMWidgetClass,
   public = list(
    #' @field _model_module Name of the Javascript module with the model
    `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _model_module_version Version of the module where the model is defined
    `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version()),sync=TRUE),
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("BoxModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("BoxView"),sync=TRUE),
    #' @field _view_module Name of the module where the view is defined
    `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
    #' @field _view_module_version Version of the module where the view is defined
    `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version()),sync=TRUE),
    #' @field children A generic vector with the widgets in the container
    children = structure(List(),sync=TRUE,auto_unbox=FALSE),
    #' @field box_style The string that describes the button style
    box_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default="")),
    #' @description An initializer function
    #' @param children A list of widgets
    #' @param ... Other arguments, passed to the superclass method
    initialize = function(children=list(),...){
        super$initialize(...)
        if(length(children)==1 && is.list(children[[1]]))
            children <- children[[1]]
        self$children <- children
        #self$send_state()
        self$on_displayed(self$notify_children_displayed)
    },
    #' @description Notifies children that they are displayed
    notify_children_displayed = function(){
        for(child in self$children)
            child$handle_displayed()
    }
   )
)

ContainerClass_new <- function(Class,layout=NULL,...){
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
    res <- do.call(Class$new,call_args)
    if(!missing(layout))
        res$layout <- layout
    return(res)
}

#' @describeIn Boxes A baseline box constructor
#' @param layout An optional layout widget
#' @param ... Arguments passed to the superclass constructor
#' @export
Box <- function(...,layout=NULL) ContainerClass_new(Class=BoxClass,layout=layout,...)

#' @rdname Boxes
#' @export
HBoxClass <- R6Class_("HBox",
    inherit = BoxClass,
    public=list(
        #' @field _model_name Name of the Javascript model in the frontend
        `_model_name` = structure(Unicode("HBoxModel"),sync=TRUE),
        #' @field _view_name Name of the Javascript model view in the frontend
        `_view_name` = structure(Unicode("HBoxView"),sync=TRUE)
    )
)

#' @describeIn Boxes A horizontal box constructor
#' @param ... Arguments passed to the superclass constructor
#' @param wrap Logical value, whether lines of widgets should be wrapped?
#' @export
HBox <- function(...,layout=NULL,wrap=FALSE) {
    box <- ContainerClass_new(Class=HBoxClass,layout=layout,...)
    if(wrap && !length(box$layout$flex_flow))
        box$layout$flex_flow <- "wrap"
    box
}

#' @rdname Boxes
#' @export
VBoxClass <- R6Class_("VBox",
    inherit = BoxClass,
    public=list(
        #' @field _model_name Name of the Javascript model in the frontend
        `_model_name` = structure(Unicode("VBoxModel"),sync=TRUE),
        #' @field _view_name Name of the Javascript model view in the frontend
        `_view_name` = structure(Unicode("VBoxView"),sync=TRUE)
    )
)

#' @describeIn Boxes A vertical box constructor
#' @param ... Arguments passed to the superclass constructor
#' @param wrap Logical value, whether lines of widgets should be wrapped?
#' @export
VBox <- function(...,layout=NULL,wrap=FALSE) {
    box <- ContainerClass_new(Class=VBoxClass,layout=layout,...)
    if(wrap && !length(box$layout$flex_flow))
        box$layout$flex_flow <- "wrap"
    box
}

#' @rdname Boxes
#' @export
GridBoxClass <- R6Class_("GridBox",
    inherit = BoxClass,
    public=list(
        #' @field _model_name Name of the Javascript model in the frontend
        `_model_name` = structure(Unicode("GridBoxModel"),sync=TRUE),
        #' @field _view_name Name of the Javascript model view in the frontend
        `_view_name` = structure(Unicode("GridBoxView"),sync=TRUE)
    )
)

#' @describeIn Boxes A grid box constructor
#' @param ... Arguments passed to the superclass constructor
#' @export
GridBox <- function(...,layout=NULL) ContainerClass_new(Class=GridBoxClass,layout=layout,...)
