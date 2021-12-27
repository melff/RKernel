#' @include widget.R widget-description.R

#' @export
SelectionWidgetClass <- R6Class_("SelectionWidget",
  inherit = ValueWidgetClass,
  public = list(
      `_options_labels` = structure(Unicode(length=NA),sync=TRUE),
      `_options_values` = list(),
      index = structure(Integer(0L,optional=TRUE,length=1L),sync=TRUE),
      value = Trait(),
      validate_index = function(index){
          # cat("validate_index")
          options_values <- self[["_options_values"]]
          len <- length(options_values)
          if(length(index) > 0){
              if(index >= len)
                  index <- len - 1L
              if(index < 0)
                  index <- 0L
          } 
          index
      },
      observe_index = function(name,self,index){
          #cat("observe_index")
          options_values <- self[["_options_values"]]
          if(length(index) > 0){
              index <- index[1]
              value <- options_values[[index + 1L]]
          } else {
              value <- NULL
          }
          self$traits$value$set(value,notify=FALSE)
          # cat(value)
          self$send_state()
      },
      validate_value = function(value){
          # cat("validate_value")
          options_labels <- self[["_options_labels"]]
          options_values <- self[["_options_values"]]
          if(length(value) > 1)
              value <- value[1]
          if(length(value) == 0){
              value <- NULL
          }
          else if(is.character(value) && value %in% options_labels){
              i <- match(value,options_labels)
              value <- options_values[i]
          }
          else if(!(value %in% options_values)){
              value <- self$value
          }
          value
      },
      observe_value = function(name,self,value){
          # cat("observe_value\n")
          # print(name)
          # cat("value\n")
          # print(value)
          if(length(value)){
              options_labels <- self[["_options_labels"]]
              options_values <- self[["_options_values"]]
              if(value %in% options_labels){
                  index <- match(value,options_labels) - 1L
              } 
              else if(value %in% options_values){
                  index <- match(value,options_values) - 1L
              }
          }
          else 
              index <- integer(0)
          self$traits$index$set(index,notify=FALSE)
          # str(self)
          self$send_state()
      },
      initialize = function(options,value,...){
          # print(options)
          # args <- list(...)
          # print(args)
          super$initialize(...)
          if(length(names(options))) {
              options <- names(options)
              self[["_options_labels"]] <- options
          }
          else {
              options <- as.character(options)
              self[["_options_labels"]] <- options
          }
          self[["_options_values"]] <- options
          self$validate("index",self$validate_index)
          self$validate("value",self$validate_value)
          self$observe("value",self$observe_value)
          self$observe("index",self$observe_index)
          if(!missing(value)){
              self$value <- value
          }
          else
              self$value <- options[[1]]
      }
  )
)

#' @export
SelectionWidget <- function(options,value,...) SelectionWidgetClass$new(options=options,value=value,...)

#' @export
DropdownClass <- R6Class_("Dropdown",
  inherit = SelectionWidgetClass,
  public = list(
      `_model_name` = structure(Unicode("DropdownModel"),sync=TRUE),
      `_view_name` = structure(Unicode("DropdownView"),sync=TRUE)
  )
)
#' @export
Dropdown <- function(options,value,...) DropdownClass$new(options=options,value=value,...)

#' @export
RadioButtonsClass <- R6Class_("RadioButtons",
  inherit = SelectionWidgetClass,
  public = list(
      `_model_name` = structure(Unicode("RadioButtonsModel"),sync=TRUE),
      `_view_name` = structure(Unicode("RadioButtonsView"),sync=TRUE)
  )
)
#' @export
RadioButtons <- function(options,value,...) RadioButtonsClass$new(options=options,value=value,...)

#' @export
ListboxSelectClass <- R6Class_("ListboxSelect",
  inherit = SelectionWidgetClass,
  public = list(
      `_model_name` = structure(Unicode("SelectModel"),sync=TRUE),
      `_view_name` = structure(Unicode("SelectView"),sync=TRUE),
      rows = structure(Integer(5L),sync=TRUE)
  )
)
#' @export
ListboxSelect <- function(options,value,...) ListboxSelectClass$new(options=options,value=value,...)


#' @export
ToggleButtonsStyleClass <- R6Class_("ToggleButtonsStyle",
  inherit = DescriptionStyleClass,
  public = list(
    `_model_name` = structure(Unicode("ToggleButtonsStyleModel"),sync=TRUE),
    button_width = structure(Unicode(character(0)),sync=TRUE),
    font_weight = structure(Unicode(character(0)),sync=TRUE)
  )
)


#' @export
ToggleButtonsClass <- R6Class_("ToggleButtons",
  inherit = SelectionWidgetClass,
  public = list(
      `_model_name` = structure(Unicode("ToggleButtonsModel"),sync=TRUE),
      `_view_name` = structure(Unicode("ToggleButtonsView"),sync=TRUE),
      tooltips = structure(Unicode(length=NA),sync=TRUE),
      icons = structure(Unicode(length=NA),sync=TRUE),
      style = structure(R6Instance(ToggleButtonsStyleClass),sync=TRUE),
      button_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default=""))
  )
)
#' @export
ToggleButtons <- function(options,value,...) ToggleButtonsClass$new(options=options,value=value,...)



#' @export
MultipleSelectionWidgetClass <- R6Class_("MultipleSelectionWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      `_options_labels` = structure(Unicode(length=NA),sync=TRUE),
      `_options_values` = list(),
      index = structure(Integer(integer(0),optional=TRUE,length=NA),sync=TRUE),
      value = Trait(),
      validate_index = function(index){
          # cat("validate_index")
          options_values <- self[["_options_values"]]
          len <- length(options_values)
          if(length(index) > 0){
              index <- unique(index)
              index[index >= len] <- len - 1L
              index[index < 0] <- 0L
          } 
          index
      },
      observe_index = function(name,self,index){
          # cat("observe_index")
          options_values <- self[["_options_values"]]
          if(length(index) > 0){
              value <- options_values[index + 1L]
          } else {
              value <- NULL
          }
          self$traits$value$set(value,notify=FALSE)
          # cat(value)
          self$send_state()
      },
      validate_value = function(value){
          # cat("validate_value")
          options_labels <- self[["_options_labels"]]
          options_values <- self[["_options_values"]]
          value <- unique(value)
          if(length(value) == 0){
              value <- NULL
          }
          else if(is.character(value) && all(value %in% options_labels)){
              i <- match(value,options_labels)
              value <- options_values[i]
          }
          else if(!all(value %in% options_values)){
              value <- self$value
          }
          value
      },
      observe_value = function(name,self,value){
          # cat("observe_value\n")
          # print(name)
          # cat("value\n")
          # print(value)
          if(length(value)){
              options_labels <- self[["_options_labels"]]
              options_values <- self[["_options_values"]]
              if(all(value %in% options_labels)){
                  index <- match(value,options_labels) - 1L
              } 
              else if(all(value %in% options_values)){
                  index <- match(value,options_values) - 1L
              }
          }
          else 
              index <- integer(0)
          self$traits$index$set(index,notify=FALSE)
          # str(self)
          self$send_state()
      },
      initialize = function(options,value,...){
          # print(options)
          # args <- list(...)
          # print(args)
          super$initialize(...)
          if(length(names(options))) {
              options <- names(options)
              self[["_options_labels"]] <- options
          }
          else {
              options <- as.character(options)
              self[["_options_labels"]] <- options
          }
          self[["_options_values"]] <- options
          self$validate("index",self$validate_index)
          self$validate("value",self$validate_value)
          self$observe("value",self$observe_value)
          self$observe("index",self$observe_index)
          if(!missing(value)){
              self$value <- value
          }
          else
              self$value <- options[[1]]
      }
  )
)

#' @export
MultipleSelectionWidget <- function(options,value,...) MultipleSelectionWidgetClass$new(options=options,value=value,...)

#' @export
ListboxSelectMultipleClass <- R6Class_("ListboxSelectMultiple",
  inherit = MultipleSelectionWidgetClass,
  public = list(
      `_model_name` = structure(Unicode("SelectMultipleModel"),sync=TRUE),
      `_view_name` = structure(Unicode("SelectMultipleView"),sync=TRUE),
      rows = structure(Integer(5L),sync=TRUE)
  )
)
#' @export
ListboxSelectMultiple <- function(options,value,...) ListboxSelectMultipleClass$new(options=options,value=value,...)
