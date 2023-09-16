#' Tag Input Widgets
#'
#' @description Classes and constructors to great tag input widgets
#'
#' @include widget-value.R
#' @name TagsInput
NULL

#' @rdname TagsInput
#' @export
TagsInputBaseClass <- R6Class_("TagsInputBase",
   inherit = ValueWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("TagsInputBaseModel"),sync=TRUE),
       #' @field value A list of tags
       value = structure(List(),sync=TRUE),
       #' @field placeholder A placeholder string
       placeholder = structure(Unicode("\U200B"),sync=TRUE),
       #' @field allowed_tags Optional list with allowed tags.
       allowed_tags = structure(List(),sync=TRUE),
       #' @field allow_duplicates Logical, whether duplicate tags are allowed.
       allow_duplicates = structure(Boolean(TRUE),sync=TRUE),
       #' @description Check value for validity,
       #' @param value A character string with one or more tags.
       validate_value = function(value){
           if(!nzchar(value)) stop("Empty strings not allowed")
           if(!allow_duplicates && anyDuplicated(value))
               stop("Duplicate tags not allowed")
           if(!length(self$allowed_tags)) return(value)
           for(tag in value){
               if(!(tag %in% allowed_tags))
                   stop(sprintf("Tag %s not allowed",tag))
           }
           return(value)
       },
       #' @description Initializer function
       #' @param ... Arguments passed to the superclass initializer
       initialize  = function(...){
           super$initialize(...)
           self$validate("value",self$validate_value)
       },
       #' @field required_version Minimum required ipywidgets version in which the
       #'        current widget class is supported.
       required_version = list(from=c(8,0,0))
   )
)

#' @rdname TagsInput
#' @export
TagsInputClass <- R6Class_("TagsInput",
   inherit = TagsInputBaseClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("TagsInputModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend.
       `_view_name` = structure(Unicode("TagsInputView"),sync=TRUE),
       #' @field value A list of tags as unicode strings
       value = structure(Unicode(length=NA),sync=TRUE,auto_unbox=FALSE),
       #' @field tag_style The string that describes the tag style
       tag_style = structure(StrEnum(
           c("primary","success","info","warning","danger",""),
           default=""),sync=TRUE)
   )                       
)


#' @describeIn TagsInput A taginput constructor
#' @param ... Arguments passed to the inializer
#' @export
TagsInput <- function(value="",...) TagsInputClass$new(value=value,...)


#' @rdname TagsInput
#' @export
ColorsInputClass <- R6Class_("ColorsInput",
   inherit = TagsInputBaseClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("ColorsInputModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend.
       `_view_name` = structure(Unicode("ColorsInputView"),sync=TRUE),
       #' @field value A list of tags as unicode strings
       value = structure(Color(length=NA),sync=TRUE,auto_unbox=FALSE)
   )                       
)


#' @describeIn TagsInput A color taginput constructor
#' @param ... Arguments passed to the inializer
#' @export
ColorsInput <- function(value="",...) ColorsInputClass$new(value=value,...)


#' @rdname TagsInput
#' @export
NumbersInputBase <- R6Class_("ColorsInput",
   inherit = TagsInputClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("NumbersInputBaseModel"),sync=TRUE),
       #' @field min A \link{Float} traitlet, the minimum allowed value.
       min = structure(Float(length=1L),sync=TRUE),
       #' @field max A \link{Float} traitlet, the maximum allowed value.
       max = structure(Float(length=1L),sync=TRUE),
       #' @field value A list of numeric tags
       value = structure(Float(length=NA),sync=TRUE,auto_unbox=FALSE),
       #' @description Check value for validity,
       #' @param value A character string with one or more tags.
       validate_value = function(value){
           if(length(self$min)){
               for(tag_val in value){
                   if(tag_val < self$min) stop("Tag value below minimum.")
               }
           }
           if(length(self$max)){
               for(tag_val in value){
                   if(tag_val > self$max) stop("Tag value below minimum.")
               }
           }
           return(value)
       },
       #' @description Initializer function
       #' @param value An initial value
       #' @param ... Arguments passed to the superclass initializer
       initialize  = function(...){
           super$initialize(...)
           self$validate("value",self$validate_value)
       }
   )                       
)

#' @rdname TagsInput
#' @export
FloatsInputClass <- R6Class_("FloatsInput",
   inherit = NumbersInputBase,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("FloatsInputModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend.
       `_view_name` = structure(Unicode("FloatsInputView"),sync=TRUE),
       #' @field value A list of numeric tags
       value = structure(Float(length=NA),sync=TRUE,auto_unbox=FALSE),
       #' @field format The number format
       format = structure(Unicode(".1f"),sync=TRUE)
   )                       
)


#' @describeIn TagsInput A color taginput constructor
#' @param ... Arguments passed to the inializer
#' @export
FloatsInput <- function(...) FloatsInputClass$new(...)


#' @rdname TagsInput
#' @export
IntsInputClass <- R6Class_("IntsInput",
   inherit = NumbersInputBase,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("IntsInputModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend.
       `_view_name` = structure(Unicode("IntsInputView"),sync=TRUE),
       #' @field min A \link{Integer} traitlet, the minimum allowed value.
       min = structure(Integer(length=1L),sync=TRUE),
       #' @field max A \link{Integer} traitlet, the maximum allowed value.
       max = structure(Integer(length=1L),sync=TRUE),
       #' @field value A list of interger number tags
       value = structure(Integer(length=NA),sync=TRUE,auto_unbox=FALSE),
       #' @field format The number format
       format = structure(Unicode("d"),sync=TRUE)
   )                       
)


#' @describeIn TagsInput A color taginput constructor
#' @param ... Arguments passed to the inializer
#' @export
IntsInput <- function(...) IntsInputClass$new(...)
