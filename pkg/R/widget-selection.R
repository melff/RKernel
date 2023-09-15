#' Selection widgets
#' @description Classes and constructors for selection widgets, i.e. dropdowns, listboxes etc.
#' @include widget.R widget-description.R
#' @name SelectionWidget

#' @rdname SelectionWidget
#' @export
SelectionWidgetClass <- R6Class_("SelectionWidget",
  inherit = ValueWidgetClass,
  public = list(
      #' @field _options_labels A unicode vector of option labels.
      `_options_labels` = structure(Unicode(length=NA),sync=TRUE),
      #' @field _options_values A list of values - the options.
      `_options_values` = list(),
      #' @field index An integer that refers to currently selected item.
      index = structure(Integer(0L,optional=TRUE,length=1L),sync=TRUE),
      #' @field value Any kind of trait(let).
      value = Trait(),
      #' @description Validate an index argument.
      #' @param index The index to be checked.
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
      #' @description Standard observer function for indices
      #' @param name Name of the traitlet
      #' @param self The traitlet container
      #' @param index The index
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
      #' @description Validate a value
      #' @param value The value to be validated
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
      #' @description Standard observer function for values
      #' @param name Name of the traitlet
      #' @param self The traitlet container
      #' @param value A value
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
      #' @description Initialiser
      #' @param options A named vector or a vector coerceable into a character vector.
      #' @param value A trait.
      #' @param ... Any other arguments, ignored.
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

#' @describeIn SelectionWidget A constructor function for selection widgets.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
SelectionWidget <- function(options,value,...) SelectionWidgetClass$new(options=options,value=value,...)

#' @rdname SelectionWidget
#' @export
DropdownClass <- R6Class_("Dropdown",
  inherit = SelectionWidgetClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("DropdownModel"),sync=TRUE),
      #' @field _view_name Name of the Javascript view in the frontend.
      `_view_name` = structure(Unicode("DropdownView"),sync=TRUE)
  )
)

#' @describeIn SelectionWidget The construction function for dropdown widgets.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
Dropdown <- function(options,value,...) DropdownClass$new(options=options,value=value,...)

#' @rdname SelectionWidget
#' @export
RadioButtonsClass <- R6Class_("RadioButtons",
  inherit = SelectionWidgetClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("RadioButtonsModel"),sync=TRUE),
      #' @field _view_name Name of the Javascript view in the frontend.
      `_view_name` = structure(Unicode("RadioButtonsView"),sync=TRUE)
  )
)

#' @describeIn SelectionWidget The construction function for radiobuttons widgets.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
RadioButtons <- function(options,value,...) RadioButtonsClass$new(options=options,value=value,...)

#' @rdname SelectionWidget
#' @export
ListboxSelectClass <- R6Class_("ListboxSelect",
  inherit = SelectionWidgetClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("SelectModel"),sync=TRUE),
      #' @field _view_name Name of the Javascript view in the frontend.
      `_view_name` = structure(Unicode("SelectView"),sync=TRUE),
      #' @field rows An integer, the number of rows.
      rows = structure(Integer(5L),sync=TRUE)
  )
)

#' @describeIn SelectionWidget The construction function for listbox-selection widgets.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
ListBox <- function(options,value,...) ListboxSelectClass$new(options=options,value=value,...)


#' @rdname SelectionWidget
#' @export
ToggleButtonsStyleClass <- R6Class_("ToggleButtonsStyle",
  inherit = DescriptionStyleClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("ToggleButtonsStyleModel"),sync=TRUE),
      #' @field button_width A unicode string, the width in CSS language
      button_width = structure(Unicode(character(0)),sync=TRUE),
      #' @field font_weight A unicode string, the font weight in CSS language
      font_weight = structure(Unicode(character(0)),sync=TRUE)
  )
)


#' @rdname SelectionWidget
#' @export
ToggleButtonsClass <- R6Class_("ToggleButtons",
  inherit = SelectionWidgetClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("ToggleButtonsModel"),sync=TRUE),
      #' @field _view_name Name of the Javascript view in the frontend.
      `_view_name` = structure(Unicode("ToggleButtonsView"),sync=TRUE),
      #' @field tooltips A unicode vector with tooltips.
      tooltips = structure(Unicode(length=NA),sync=TRUE),
      #' @field icons A unicode vector with icon specs.
      icons = structure(Unicode(length=NA),sync=TRUE),
      #' @field style A TobbleButtonStyle widget
      style = structure(R6Instance(ToggleButtonsStyleClass),sync=TRUE),
      #' @field button_style A character string, one of "primary", "success", "info",
      #'        "warning", "danger", or the empty string.
      button_style = structure(StrEnum(
        c("primary","success","info","warning","danger",""),
        default=""))
  )
)

#' @describeIn SelectionWidget The construction function for togglebuttons widgets.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
ToggleButtons <- function(options,value,...) ToggleButtonsClass$new(options=options,value=value,...)



#' @rdname SelectionWidget
#' @export
MultipleSelectionWidgetClass <- R6Class_("MultipleSelectionWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      #' @field _options_labels A unicode string vector with labels
      `_options_labels` = structure(Unicode(length=NA),sync=TRUE),
      #' @field _options_values A list of labelled values
      `_options_values` = list(),
      #' @field index An integer vector of indices of currenlty selected elements.
      index = structure(Integer(integer(0),optional=TRUE,length=NA),sync=TRUE),
      #' @field value A traitlet vector.
      value = Trait(),
      #' @description Validate an index.
      #' @param index An index, the index to be checked.
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
      #' @description Standard observer function for indices
      #' @param name Name of the traitlet
      #' @param self The traitlet container
      #' @param index The index
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
      #' @description Validate a value
      #' @param value The value to be validated
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
      #' @description Standard observer function for values
      #' @param name Name of the traitlet
      #' @param self The traitlet container
      #' @param value A value
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
      #' @description Initialiser
      #' @param options A named vector or a vector coerceable into a character vector.
      #' @param value A trait.
      #' @param ... Any other arguments, ignored.
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

#' @describeIn SelectionWidget The construction function for multiple-selection widgets.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
MultipleSelectionWidget <- function(options,value,...) MultipleSelectionWidgetClass$new(options=options,value=value,...)

#' @rdname SelectionWidget
#' @export
ListboxSelectMultipleClass <- R6Class_("ListboxSelectMultiple",
  inherit = MultipleSelectionWidgetClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("SelectMultipleModel"),sync=TRUE),
      #' @field _view_name Name of the Javascript view in the frontend.
      `_view_name` = structure(Unicode("SelectMultipleView"),sync=TRUE),
      #' @field rows An integer, the number of rows.
      rows = structure(Integer(5L),sync=TRUE)
  )
)

#' @describeIn SelectionWidget The construction function for listbox widgets with multiple selections.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
ListBoxMultiple <- function(options,value,...) ListboxSelectMultipleClass$new(options=options,value=value,...)

