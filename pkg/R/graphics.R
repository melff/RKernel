#' @importFrom grDevices png jpeg svg pdf
mime_graphics <- function(plt,mime,width,height,pointsize,scale,res,units="in"){
    grapics_data <- NULL
    if(mime=="image/png"){
        tf <- tempfile(fileext=".png")
        png(filename=tf,
            width=width,
            height=height,
            units=units,
            res=res/scale)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    else if(mime=="image/jpeg"){
        tf <- tempfile(fileext=".jpeg")
        jpeg(filename=tf,
            width=width,
            height=height,
            units=units,
            res=res/scale)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    else if(mime=="image/svg+xml"){
        tf <- tempfile(fileext=".svg")
        svg(filename=tf,
            width=width,
            height=height)
        replayPlot(plt)
        dev.off()
        graphics_data <- readChar(tf, file.info(tf)$size,useBytes=TRUE)
    }
    else if(mime=="application/pdf"){
        tf <- tempfile(fileext=".pdf")
        pdf(file=tf,
            width=width,
            height=height)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    return(graphics_data)
}

graphics <- new.env()

GraphicsDevice <- R6Class("GraphicsDevice",
    public = list(
        initialize = function(evaluator){
            private$evaluator <- evaluator
            os <- .Platform$OS.type
            sysname <- Sys.info()[["sysname"]]
            if(os == "unix" && sysname=="Darwin")
                os <- "osx"
            private$dev_filename <- switch(os,
                                           windows="NUL",
                                           osx=NULL,
                                           unix="/dev/null")
            private$dev_name <- switch(os,
                                       windows="png",
                                       osx="pdf",
                                       unix="png")
            setHook('plot.new',private$plot_new_hook)
            setHook('grid.newpage',private$plot_new_hook)
            setHook('before.plot.new',private$before_plot_new_hook)
            setHook('before.grid.newpage',private$before_plot_new_hook)
            graphics$current <- self
            private$device()
            private$empty_plot <- recordPlot()
        },
        is_active = function(){
            private$dev_num == dev.cur()
        },
        activate = function(){
            private$device()
        },
        new_page = function(reset=FALSE){
            result <- isTRUE(private$plot_new_called)
            if(reset)
                private$plot_new_called <- FALSE
            return(result)
        },
        clear = function(){
            replayPlot(private$empty_plot)
        },
        push = function(plt){
            n <- length(private$plot_stack)
            private$plot_stack[[n+1]] <- recordPlot()
            if(inherits(plt,"recordedplot"))
                replayPlot(plt)
            else
                replayPlot(private$empty_plot)
        },
        pop = function(){
            n <- length(private$plot_stack)
            retval <- recordPlot()
            plt <- private$plot_stack[[n]]
            replayPlot(plt)
            private$plot_stack[[n]] <- NULL
        }
    ),
    private = list(

        evaluator = NULL,
        plot_new_called = FALSE,
        graphics_par_usr = numeric(0),

        before_plot_new_hook = function(...){
            if(self$is_active()){
                graphics_hooks$before_plot_new$run()
                # dev.control(displaylist="enable")
            }
        },

        plot_new_hook = function(...){
            # log_out("plot_new_hook")
            # log_out("private$dev_num==",private$dev_num)
            # log_out("dev.cur()==",dev.cur())
            if(self$is_active()){
                graphics_hooks$plot_new$run()
                private$plot_new_called <- TRUE
                private$graphics_par_usr <- par("usr")
            } #else log_out("graphics not active ...")
        },

        dev_filename = character(0),
        dev_name = character(),
        dev_num = 0,
        device = function(filename = NULL,
                          width = getOption("jupyter.plot.width",6),
                          height = getOption("jupyter.plot.height",6),
                          res = getOption("jupyter.plot.res",96),
                          pointsize = getOption("jupyter.plot.pointsize",12),
                          units = getOption("jupyter.plot.units","in"),
                          ...){
            dev <- get(private$dev_name)
            if(is.null(filename))
                dev(filename=private$dev_filename,
                    width=width,
                    height=height,
                    res=res,
                    units=units,
                    ...)
            private$dev_num <- dev.cur()
            dev.control(displaylist="enable")
            
            # log_out('private$device')
            # log_out("private$dev_num==",private$dev_num)

        },
        empty_plot = NULL,
        plot_stack = NULL
    )
)

