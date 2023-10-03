#' Selection widgets
#' @description Classes and constructors for selection widgets, i.e. dropdowns, listboxes etc.
#' @include widget.R widget-description.R
#' @name SelectionWidget

#' @rdname SelectionWidget
#' @export
SelectionWidgetClass <- R6Class_("SelectionWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      #' @field _options_labels A unicode vector of option labels.
      `_options_labels` = structure(Unicode(length=NA),sync=TRUE),
      #' @field index An integer that refers to currently selected item.
      index = structure(Integer(0L,optional=TRUE,length=1L),sync=TRUE),
      #' @description Validate an index argument.
      #' @param index The index to be checked.
      validate_index = function(index){
          # log_out("validate_index")
          options_labels <- self[["_options_labels"]]
          len <- length(options_labels)
          if(length(index) > 0){
              index <- index[1]
              if(index >= len)
                  index <- len - 1L
              if(index < 0)
                  index <- 0L
          } 
          else index <- self$index
          index
      },
      #' @description Initialiser
      #' @param options A named vector or a vector coerceable into a character vector.
      #' @param value Name of a selectable option.
      #' @param ... Any other arguments, ignored.
      initialize = function(options,value,...){
          super$initialize(...)
          options <- as.character(options)
          self[["_options_labels"]] <- options
          self$validate("index",self$validate_index)
          if(!missing(value)){
              value <- value[1L]
              index <- match(value,options,nomatch=0L)
              index <- index[index > 0L]
              if(length(index))
                  self$index <- index - 1L
          }
      }
  ),
  active = list(
      #' @field value The selected option
      value = function(value){
          if(missing(value)){
              index <- self$traits$index$value + 1L
              options_labels <- self[["_options_labels"]]
              options_labels[index]
          } else {
              options_labels <- self[["_options_labels"]]
              index <- match(value,options_labels,nomatch=0L)
              index <- index[index > 0L]
              if(length(index)){
                  self$traits$index$set(index[1L] - 1L)
                  self$send_state()
              }
          }
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
#' @include widget-slider.R
#' @export
SelectionSliderClass <- R6Class_("SelectionSlider",
  inherit = SelectionWidgetClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("SelectionSliderModel"),sync=TRUE),
      #' @field _view_name Name of the Javascript view in the frontend.
      `_view_name` = structure(Unicode("SelectionSliderView"),sync=TRUE),
       #' @field orientation A Unicode string, either "horizontal" or "vertical"
       orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
       #' @field readout A logical value, whether the value should be showns (read out)
       readout = structure(Boolean(TRUE),sync=TRUE),
       #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
       continuous_update = structure(Boolean(TRUE),sync=TRUE),
       #' @field style A SliderStyle widget
       style = structure(R6Instance(SliderStyleClass),sync=TRUE),
       #' @field behavior A string that describes the ddragging behavior.
       behavior = structure(StrEnum(c("drag-tap","drag-snap","tap","drag","snap")),sync=has_iw_ver(8))
  )
)

#' @describeIn SelectionWidget The construction function for listbox widgets with multiple selections.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
SelectionSlider <- function(options,value,...) SelectionSliderClass$new(options=options,value=value,...)



#' @rdname SelectionWidget
#' @export
MultipleSelectionWidgetClass <- R6Class_("MultipleSelectionWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      #' @field _options_labels A unicode string vector with labels
      `_options_labels` = structure(Unicode(length=NA),sync=TRUE),
      #' @field index An integer vector of indices of currenlty selected elements.
      index = structure(Integer(integer(0),optional=TRUE,length=NA),sync=TRUE),
      #' @description Validate an index.
      #' @param index An index, the index to be checked.
      validate_index = function(index){
          # log_out("MultipleSelectionWidgetClass$validate_index")
          # log_out(index,use.print=TRUE)
          options_labels <- self[["_options_labels"]]
          len <- length(options_labels)
          if(length(index) > 0){
              index <- unique(index)
              index[index >= len] <- len - 1L
              index[index < 0] <- 0L
          } 
          sort(index)
      },
      #' @description Initialiser
      #' @param options A named vector or a vector coerceable into a character vector.
      #' @param value Names of selectable options.
      #' @param ... Any other arguments, ignored.
      initialize = function(options,value,...){
          # log_out("MultipleSelectionWidgetClass$validate_index")
          # super$initialize(...)
          options <- as.character(options)
          self[["_options_labels"]] <- options
          self$validate("index",self$validate_index)
          if(!missing(value)){
              index <- match(value,options,nomatch=0L)
              index <- index[index > 0L]
              self$index <- index - 1L
          }
      }
  ),
  active = list(
      #' @field value The selected option
      value = function(value){
          if(missing(value)){
              index <- self$traits$index$value + 1L
              options_labels <- self[["_options_labels"]]
              options_labels[index]
          } else {
              options_labels <- self[["_options_labels"]]
              index <- match(value,options_labels,nomatch=0L)
              index <- index[index > 0L]
              if(length(index)){
                  self$traits$index$set(index - 1L)
                  self$send_state()
              }
          }
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

#' @rdname SelectionWidget
#' @include widget-slider.R
#' @export
SelectionRangeSliderClass <- R6Class_("SelectionRangeSlider",
  inherit = MultipleSelectionWidgetClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("SelectionRangeSliderModel"),sync=TRUE),
      #' @field _view_name Name of the Javascript view in the frontend.
      `_view_name` = structure(Unicode("SelectionRangeSliderView"),sync=TRUE),
      #' @field orientation A Unicode string, either "horizontal" or "vertical"
      orientation = structure(StrEnum(c("horizontal","vertical"),default="horizontal"),sync=TRUE),
      #' @field readout A logical value, whether the value should be showns (read out)
      readout = structure(Boolean(TRUE),sync=TRUE),
      #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
      continuous_update = structure(Boolean(TRUE),sync=TRUE),
      #' @field style A SliderStyle widget
      style = structure(R6Instance(SliderStyleClass),sync=TRUE),
      #' @field behavior A string that describes the ddragging behavior.
      behavior = structure(StrEnum(c("drag-tap","drag-snap","tap","drag","snap")),sync=has_iw_ver(8)),
      #' @description Validate an index.
      #' @param index An index, the index to be checked.
      validate_index = function(index){
          index <- as.integer(range(index))
          index
      },
      #' @description Initialiser
      #' @param options A named vector or a vector coerceable into a character vector.
      #' @param value Names of selectable options.
      #' @param ... Any other arguments, ignored.
      initialize = function(options,value,...){
          super$initialize(options,value,...)
          index <- self$traits$index$get()
          index <- self$validate_index(index)
          self$traits$index$set(index)
          # self$validate("index",self$validate_index)
      }
  )
)

#' @describeIn SelectionWidget The construction function for listbox widgets with multiple selections.
#' 
#' @param options A named vector or a vector coerceable into a character vector.
#' @param value A trait.
#' @param ... Any other arguments, ignored.
#' @export
SelectionRangeSlider <- function(options,value,...) SelectionRangeSliderClass$new(options=options,value=value,...)
