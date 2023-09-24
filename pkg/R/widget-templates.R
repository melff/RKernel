#' @export
TemplateBaseClass <- R6Class_("TemplateBase",
    inherit = GridBoxClass,
    public = list(
        #' @field grid_gap The grid-gap CSS attribute
        grid_gap = Unicode(optional=TRUE),
        #' @field justify_content The justify-content CSS attribute
        justify_content = StrEnum(c("flex-start","flex-end","center",
                                    "space-between","space-around"),optional=TRUE),
        #' @field align_items The align-items CSS attribute
        align_items = StrEnum(c("top","bottom",
                                "flex-start","flex-end","center",
                                "baseline","scretch"),optional=TRUE),
        #' @field width The width CSS attribute
        width = Unicode(optional=TRUE),
        #' @field height The height CSS attribute
        height = Unicode(optional=TRUE),
        #' @description Initializer
        #' @param ... Arguments, used to initialize the fields
        initialize = function(...){
            super$initialize(...)
            private$copy_layout_props()
            private$set_observers()
        }
    ),
    private = list(
        properties = c("grid_gap","justify_content","align_items","width","height"),
        property_mapping = NULL,
        property_rewrite = function(prop,value){
            if(!prop %in% names(private$property_mapping)) return(value)
            else {
                map <- property_mapping[prop]
                n <- intersect(names(map),value)
                i <- match(value,n)
                value[i] <- map[n]
                return(value)
            }
        },
        copy_layout_props = function(){
            for(prop in private$properties)
                private$sync_layout_prop(prop)
        },
        sync_layout_prop = function(prop){
            value <- self[[prop]]
            value <- private$property_rewrite(prop,value)
            if(length(value))
                self$layout[[prop]] <- value
        },
        set_observers = function(){
            for(prop in private$properties){
                self$observe(prop,function(prop,self,value){
                    private$sync_layout_prop(prop)
                })
            }
        }
    )
)
