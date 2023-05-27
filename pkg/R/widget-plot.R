#' Widgets to receive plots graphics
#' @description Class and constructors show graphics created by code
#' @include widget-media.R
#' @name PlotWidget


#' @rdname PlotWidget
#' @importFrom svglite svgstring
#' @export
SVGWidgetClass <- R6Class_(
    "SVGWidget",
    inherit = HTMLClass,
    public = list(
        #' @field context A Context instance or NULL
        context = NULL,
        #' @description 
        #' Initialize the object
        #' @param ... Arguments passed to the superclass initializer
        #' @param width A character string, giving the width as a CSS property
        #' @param height A character string, giving the height as a CSS property
        #' @param envir An optional environment within which expressions are evaluated
        initialize = function(...,
                              width=NULL,
                              height=NULL,
                              envir = new.env()){
            super$initialize(...)
            context <- Context$new(envir=self$envir,
                                   attachment=list(
                                       display=self$display
                                   ))
            context$on_enter(private$enter)
            context$on_exit(private$exit)

            context$on_eval(private$handle_graphics)
            self$context <- context
            private$kernel <- get_current_kernel()
            private$dev_num_other <- dev.cur()
            private$new_device()
            private$dev_num <- dev.cur()
            dev.set(private$dev_num_other)
            style <- character(0)
            if(length(width)){
                style <- c(style,paste0("width:",width))
            }
            if(length(height)){
                style <- c(style,paste0("height:",height))
            }
            if(length(style)){
                style <- paste0(style,collapse=";")
                private$style <- style
            }
        }
    ),
    private = list(
        style = NULL,
        handle_graphics = function(){
            string <- private$svg_string()
            if(length(string)>1){
                string <- string[length(string)]
            }
            string <- private$set_dims(string)
            self$value <- string
        },
        enter = function(){
            private$dev_num_other <- dev.cur()
            dev.set(private$dev_num)
        },
        exit = function(){
            dev.set(private$dev_num_other)
        },
        kernel = NULL,
        dev_num = 0,
        dev_num_other = 0,
        svg_string = NULL,
        new_device = function(){
            width <- getOption("jupyter.plot.width",6)
            height <- getOption("jupyter.plot.height",6)
            pointsize <- getOption("jupyter.plot.pointsize",12)
            scale <- getOption("jupyter.plot.scale",0.5)
            private$svg_string <- svgstring(width=width,height=height,pointsize=pointsize,
                                            standalone=FALSE)
        },
        set_dims = function(string){
            if(length(private$style)){
                pattern <- "(<svg.*?)(>)"
                replacement <- paste0("\\1 style=",private$style,"\\2")
                string <- sub(pattern,replacement,string)
            }
            return(string)
        }
    )
)

#' @rdname PlotWidget
#' @param ... Arguments, passed to the ImageWidget constructors.
#' @export
SVGWidget <- function(...) SVGWidgetClass$new(...)

#' @rdname PlotWidget
#' @param data An "SVGWidget" object
#' @param expr An expression to evaluate, or a sequence of expression, 
#'    encapsulated by curly braces.
#' @param enclos An enclosing environment.
#' @export
with.SVGWidget <- function(data,expr,enclos=parent.frame(),...)
    data$context$eval(substitute(expr),enclos=enclos)


#' @rdname PlotWidget
#' @param ... Arguments, passed to the ImageWidget constructors.
#' @export
PlotWidget <- function(...) SVGWidgetClass$new(...)

