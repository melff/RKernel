#' @importFrom uuid UUIDgenerate
#' @include widget-dom.R

#' @export
OutputWidgetClass <- R6Class_("OutputWidget",
    inherit = DOMWidgetClass,
    public = list(
        `_view_name` = structure(Unicode("OutputView"),sync=TRUE),
        `_model_name` = structure(Unicode("OutputModel"),sync=TRUE),
        `_view_module` = structure(Unicode("@jupyter-widgets/output"),sync=TRUE),
        `_model_module` = structure(Unicode("@jupyter-widgets/output"),sync=TRUE),
        `_view_module_version` = structure(Unicode(jupyter_widgets_output_version),sync=TRUE),
        `_model_module_version` = structure(Unicode(jupyter_widgets_output_version),sync=TRUE),

        msg_id = structure(Unicode(""),sync=TRUE),

        context = NULL,
        envir = NULL,
        append_output = TRUE,
        graphics_widget = NULL,
        
        initialize = function(append_output = TRUE,
                              graphics_widget = NULL,
                              ...){
            super$initialize(...)
            if(inherits(graphics_widget,"ImageWidget")){
                self$graphics_widget <- graphics_widget
                handle_graphics <- self$handle_graphics_image
            }
            else
                handle_graphics <- self$handle_graphics_display
            context <- Context$new(text_callback=self$handle_text,
                                        message_callback=self$handle_message,
                                        warning_callback=self$handle_warning,
                                        error_callback=self$handle_error,
                                        value_callback=self$handle_value,
                                        graphics_callback=handle_graphics,
                                        envir=new.env(),
                                        attachment=list(
                                            display=self$display
                                        ))
            context$on_enter(self$enter)
            context$on_exit(self$exit)
            self$context <- context
            self$envir <- context$envir
            self$append_output <- append_output
        },

        enter = function(){
            kernel <- get_current_kernel()
            self$msg_id <- kernel$current_request$header$msg_id
            # log_out(sprintf("msg_id set to '%s'",self$msg_id))
        },
        exit = function(){
            self$msg_id <- ""
        },
        
        do = function(...) self$context$do(...),
        eval = function(expr) self$context$eval(expr),
        evaluate = function(expressions) self$context$evaluate(expressions),

        handle_text = function(text) {
            text <- paste(text,collapse="\n")
            self$stream(text = text,
                        stream = "stdout")
        },

        handle_graphics_image = function(plt,update=FALSE){
            graphics_widget <- self$graphics_widget
            if(!inherits(graphics_widget,"ImageWidget")) return()
            width      <- getOption("jupyter.plot.width",6)
            height     <- getOption("jupyter.plot.height",6)
            pointsize  <- getOption("jupyter.plot.pointsize",12)
            resolution <- getOption("jupyter.plot.res",150)
            scale      <- getOption("jupyter.plot.scale",0.5)
            units      <- getOption("jupyter.plot.units","units")
            Cairo(type="raster",
                  width=width,
                  height=height,
                  units=units,
                  dpi=resolution/scale)
            replayPlot(plt)
            raster <- Cairo.capture()
            dev.off()
            graphics_widget$value <- writePNG(raster)
        },
        
        last_plot_id = character(0),
        handle_graphics_display = function(plt,update=FALSE) {

            update <- update && getOption("jupyter.update.graphics",TRUE)
            # log_out(sprintf("OutputWidget$handle_graphics(...,update=%s)",if(update)"TRUE"else"FALSE"))

            width      <- getOption("jupyter.plot.width",6)
            height     <- getOption("jupyter.plot.height",6)
            pointsize  <- getOption("jupyter.plot.pointsize",12)
            resolution <- getOption("jupyter.plot.res",150)
            scale      <- getOption("jupyter.plot.scale",0.5)
            units      <- getOption("jupyter.plot.units","units")

            rkernel_graphics_types <- getOption("jupyter.graphics.types")

            # log_out("OutputWidget$graphics_send")
            # log_out("  -- update = ",if(update)"TRUE"else"FALSE")
            # log_out("  -- id = ",self$last_plot_id)

            update <- update && (length(self$last_plot_id)>0)
            if(update){
                id <- self$last_plot_id
            } 
            else {
                id <- UUIDgenerate()
                self$last_plot_id <- id
            } 
            # log_out("  -- update = ",if(update)"TRUE"else"FALSE")
            # log_out("  -- id = ",id)

            d <- display_data(plt,
                              width=width,
                              height=height,
                              pointsize=pointsize,
                              resolution=resolution,
                              scale=scale,
                              units=units,
                              id=id,
                              update=update)
            self$display_send(d)
        },

        handle_message = function(m) {
            text <- conditionMessage(m)
            text <- paste(text,collapse="\n")
            self$stream(text = text,
                        stream = "stdout")
        },

        handle_warning = function(w) {
            text <- conditionMessage(w)
            text <- paste(text,collapse="\n")
            call <- conditionCall(w)
            if(is.null(call)) {
                text <- paste0("Warning:\n",text,"\n")
            } else {
                call <- deparse(call)[[1]]
                text <- paste0("Warning in ",call,":\n",text,"\n")
            }
            self$stream(text = text,
                        stream = "stderr")
        },

        handle_error = function(e) {
            text <- conditionMessage(e)
            text <- paste(text,collapse="\n")
            call <- conditionCall(e)
            if(is.null(call)) {
                text <- paste0("Error:\n",text,"\n")
            } else {
                call <- deparse(call)[[1]]
                text <- paste0("Error in ",call,":\n",text,"\n")
            }
            self$stream(text = text,
                        stream = "stderr")
        },

        handle_value = function(x,visible) {
            if(visible){
                if(any(class(x) %in% getOption("rkernel_displayed_classes"))){
                    d <- display_data(x)
                    self$display_send(d)
                }
                else if(inherits(x,"display_data")){
                    self$display_send(x)
                }
                else if(inherits(x,"update_display_data")){
                    self$display_send(x)
                }
                else {
                    text <- capture.output(print(x))
                    text <- paste(c("",text),collapse="\n")
                    self$stream(text = text,
                                stream = "stdout")
                }
            }
        },
        cat = function(...,sep=" "){
            text <- paste(...,sep=sep)
            self$stream(text,"stdout")
        },
        print = function(x,...){
            text <- capture.output(print(x,...))
            self$stream(text,"stdout")
        },
        stdout = function(text) self$stream(text,"stdout"),
        stderr = function(text) self$stream(text,"stderr"),

        outputs = structure(List(),sync=TRUE),
        current_output = NULL,
        stream = function(text,stream_name) {
            if(!nzchar(text)) return()
            if(self$append_output){
                if(!is.null(self$current_output) &&
                   identical(self$current_output$output_type, "stream") &&
                   identical(self$current_output$name, stream_name)){
                    self$current_output$text <- paste0(self$current_output$text,
                                                       text)
                    l <- length(self$outputs)
                    self$outputs[[l]] <- self$current_output
                }
                else {
                    self$current_output <- list(
                        output_type = "stream",
                        name = stream_name,
                        text = text
                    )
                    outputs <- self$outputs
                    l <- length(outputs)
                    outputs[[l+1]] <- self$current_output
                    self$outputs <- outputs
                }
            }
            else {
                self$current_output <- list(
                    output_type = "stream",
                    name = stream_name,
                    text = text
                )
                self$outputs <- list(self$current_output)
            }
        },
        display = function(...){
            d <- display_data(...)
            self$display_send(d)
        },
        display_index = integer(0),
        display_send = function(d){
            if(!(class(d)%in%c("display_data","update_display_data")))
                stop("'display_data' or 'update_display_data' object required")
            out_data <- list(output_type = "display_data",
                             data = d$data,
                             metadata = d$metadata)
            if(self$append_output){
                id <- d$transient$display_id
                update <- inherits(d,"update_display_data")
                l <- length(self$display_index)
                # log_out(id,use.print=TRUE)
                # log_out("OutputWidget$display_send")
                # log_out(" -- id = ",id)
                # log_out(self$display_index,use.print=TRUE)
                if(update){
                    if(id == "last" && l > 0){
                        i <- self$display_index[l]
                        self$outputs[[i]] <- out_data
                    }
                    if(id %in% names(self$display_index)){
                        i <- self$display_index[id]
                        self$outputs[[i]] <- out_data
                    }
                } else {
                    if(l < 1 || !(id %in% names(self$display_index))){
                        i <- length(self$outputs) + 1L
                        ii <- l + 1L
                        self$display_index[ii] <- i
                        names(self$display_index)[ii] <- id
                        self$outputs[[i]] <- out_data
                        self$current_output <- out_data
                    }
                }
            }
            else {
                self$outputs <- list(out_data)
                self$current_output <- out_data
            }
        },
        last_display = function(){
            l <- length(self$display_index)
            if(l > 0){
                names(self$display_index[l])
            }
            else
                character(0)
        },
        clear = function(){
            self$display_index <- integer(0)
            self$outputs <- list()
        }
    )
)

#' @export
OutputWidget <- function(...) OutputWidgetClass$new(...)


#' @export
with.OutputWidget <- function(data,expr,enclos=parent.frame(),...)
    data$context$eval(substitute(expr),enclos=enclos)
