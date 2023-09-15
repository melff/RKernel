#' Tabs and Accordions
#'
#' @description Classes and constructor functions for tab and accordion widgets
#' 
#' @include widget.R widget-box.R
#' @name SelectionContainer
NULL

#' @rdname SelectionContainer 
#' @export
SelectionContainerClass <- R6Class_("Box",
   inherit = BoxClass,
   public = list(
       #' @field _titles A dictionary of strings, for internal use only
       `_titles`=structure(Dict(),sync=TRUE),
       #' @field selected_index An integer, the field currently selected.
       selected_index = structure(Integer(0L),sync=TRUE),
       #' @description Validate the index, i.e. check whether it is within range.
       #' @param index An integer number.
       validate_index = function(index){
           len <- length(self$children)
           if(index < 0 || index >= len)
               stop("index out of range")
           index
       },
       #' @description Set the title of one of the elements.
       #' @param index The index number of the element to be changed.
       #' @param title A character string, the intended title.
       set_title = function(index,title){
           index <- sprintf("%d",as.integer(index))
           self[["_titles"]][[index]] <- title
           self$send_state("_titles")
       },
       #' @description Get the title of one of the elements.
       #' @param index The index number of the element to be enquired.
       get_title = function(index){
           index <- sprintf("%d",as.integer(index))
           self[["_titles"]][[index]]
       }
   )
)

#' @rdname SelectionContainer
#' @export
AccordionClass <- R6Class_("Accordion",
    inherit = SelectionContainerClass,
    public=list(
        #' @field _model_name Name of the Javascript model in the frontend.
        `_model_name` = structure(Unicode("AccordionModel"),sync=TRUE),
        #' @field _view_name Name of the Javascript view in the frontend.
        `_view_name` = structure(Unicode("AccordionView"),sync=TRUE)
    )
)

#' @describeIn SelectionContainer The constructor function for accordion widgets.
#' @param ... Arguments passed to the superclass constructor
#' @export
Accordion <- function(...) ContainerClass_new(Class=AccordionClass,...)

#' @rdname SelectionContainer 
#' @export
TabClass <- R6Class_("Tab",
    inherit = SelectionContainerClass,
    public=list(
        #' @field _model_name Name of the Javascript model in the frontend.
        `_model_name` = structure(Unicode("TabModel"),sync=TRUE),
        #' @field _view_name Name of the Javascript view in the frontend.
        `_view_name` = structure(Unicode("TabView"),sync=TRUE)
    )
)

#' @describeIn SelectionContainer The construction function for accordion widgets.
#' @param ... Arguments passed to the superclass constructor
#' @export
Tab <- function(...) ContainerClass_new(Class=TabClass,...)

#' @rdname SelectionContainer 
#' @export
StackClass <- R6Class_("Stack",
    inherit = SelectionContainerClass,
    public=list(
        #' @field _model_name Name of the Javascript model in the frontend.
        `_model_name` = structure(Unicode("StackModel"),sync=TRUE),
        #' @field _view_name Name of the Javascript view in the frontend.
        `_view_name` = structure(Unicode("StackView"),sync=TRUE)
    )
)

#' @describeIn SelectionContainer The construction function for accordion widgets.
#' @param ... Arguments passed to the superclass constructor
#' @export
Stack <- function(...) ContainerClass_new(Class=StackClass,...)
