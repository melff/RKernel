#' Widgets to receive plots graphics
#' @description Classes and constructors show graphics created by code
#' @include widget-media.R
#' @name PlotWidget

#' @rdname PlotWidget
#' @export
PlotWidgetClass <- R6Class_(
    "PlotWidget",
    inherit = ImageWidgetClass,
    public = list(
        context = NULL,
        graphics = NULL,
        initialize = function(...,
                              envir = new.env()){
            super$initialize(...)
            context <- Context$new(envir=self$envir,
                                   attachment=list(
                                       display=self$display
                                   ))
            context$on_enter(self$enter)
            context$on_exit(self$exit)

            context$on_eval(self$handle_graphics)
            self$context <- context
            self$value <- empty20x20
            private$kernel <- get_current_kernel()
            self$graphics <- GraphicsDevice$new()
        },
        handle_graphics = function(){

            if(!self$graphics$is_active()) return(NULL)
            plt <- self$graphics$get_plot()
            if(!length(plt)) return(NULL)
            
            width      <- getOption("jupyter.plot.width",6)
            height     <- getOption("jupyter.plot.height",6)
            pointsize  <- getOption("jupyter.plot.pointsize",12)
            res        <- getOption("jupyter.plot.res",150)
            scale      <- getOption("jupyter.plot.scale",0.5)
            units      <- getOption("jupyter.plot.units","units")

            self$width <- width*res*scale
            self$height <- height*res*scale
            
            self$value <- mime_graphics(plt,
                                        mime="image/png",
                                        width=width,
                                        height=height,
                                        pointsize=pointsize,
                                        scale=scale,
                                        res=res,
                                        units=units)
        },
        enter = function(){
            parent <- private$kernel$get_parent("shell")
            self$msg_id <- parent$header$msg_id
            self$graphics$activate()
        },
        exit = function(){
            self$msg_id <- ""
            self$graphics$suspend()
        }
    ),
    private = list(
        kernel = NULL
    )
)

#' @rdname PlotWidget
#' @param ... Arguments, passed to the ImageWidget constructors.
#' @export
PlotWidget <- function(...) PlotWidgetClass$new(...)

#' @rdname PlotWidget
#' @param data An "PlotWidget" object
#' @param expr An expression to evaluate, or a sequence of expression, 
#'    encapsulated by curly braces.
#' @param enclos An enclosing environment.
#' @export
with.PlotWidget <- function(data,expr,enclos=parent.frame(),...)
    data$context$eval(substitute(expr),enclos=enclos)





#' Widgets to receive plots graphics
#' @description Classes and constructors show graphics created by code
#' @include widget-media.R
#' @name SVGWidget

#' @rdname SVGWidget
#' @importFrom svglite svgstring
#' @export
SVGWidgetClass <- R6Class_(
    "SVGWidget",
    inherit = HTMLClass,
    public = list(
        context = NULL,
        initialize = function(...,
                              width=NULL,
                              height=NULL,
                              envir = new.env()){
            super$initialize(...)
            context <- Context$new(envir=self$envir,
                                   attachment=list(
                                       display=self$display
                                   ))
            context$on_enter(self$enter)
            context$on_exit(self$exit)

            context$on_eval(self$handle_graphics)
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
                self$style <- style
            }
        },
        handle_graphics = function(){
            if(private$use.recording){
                plt <- recordPlot()
                if(!length(plt)) return(NULL)
                private$new_device()
                replayPlot(plt)
                dev.off()
            }
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
        style = NULL
    ),
    private = list(
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
        use.recording = FALSE,
        widht = NULL,
        height = NULL,
        set_dims = function(string){
            if(length(self$style)){
                pattern <- "(<svg.*?)(>)"
                replacement <- paste0("\\1 style=",self$style,"\\2")
                string <- sub(pattern,replacement,string)
            }
            return(string)
        }
    )
)

#' @rdname SVGWidget
#' @param ... Arguments, passed to the ImageWidget constructors.
#' @export
SVGWidget <- function(...) SVGWidgetClass$new(...)

#' @rdname SVGWidget
#' @param data An "SVGWidget" object
#' @param expr An expression to evaluate, or a sequence of expression, 
#'    encapsulated by curly braces.
#' @param enclos An enclosing environment.
#' @export
with.SVGWidget <- function(data,expr,enclos=parent.frame(),...)
    data$context$eval(substitute(expr),enclos=enclos)
