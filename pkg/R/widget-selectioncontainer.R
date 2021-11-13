#' @include widget.R widget-box.R

#' @export
SelectionContainerClass <- R6Class_("Box",
   inherit = BoxClass,
   public = list(
       `_titles`=structure(Dict(),sync=TRUE),
       selected_index = structure(Integer(0L),sync=TRUE),
       validate_index = function(index){
           len <- length(self$children)
           if(index < 0 || index >= len)
               stop("index out of range")
           index
       },
       set_title = function(index,title){
           index <- sprintf("%d",as.integer(index))
           self[["_titles"]][[index]] <- title
           self$send_state("_titles")
       },
       get_title = function(index){
           index <- sprintf("%d",as.integer(index))
           self[["_titles"]][[index]]
       }
   )
)

#' @export
AccordionClass <- R6Class_("Accordion",
    inherit = SelectionContainerClass,
    public=list(
        `_model_name` = structure(Unicode("AccordionModel"),sync=TRUE),
        `_view_name` = structure(Unicode("AccordionView"),sync=TRUE)
    )
)

#' @export
Accordion <- function(...) ContainerClass_new(Class=AccordionClass,...)

#' @export
TabClass <- R6Class_("Tab",
    inherit = SelectionContainerClass,
    public=list(
        `_model_name` = structure(Unicode("TabModel"),sync=TRUE),
        `_view_name` = structure(Unicode("TabView"),sync=TRUE)
    )
)

#' @export
Tab <- function(...) ContainerClass_new(Class=TabClass,...)
