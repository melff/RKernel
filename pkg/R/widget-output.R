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
        outputs = structure(List(),sync=TRUE),

        context = NULL,
        envir = NULL,

        initialize = function(...){
            super$initialize(...)
            self$envir <- new.env()
            self$context <- Context$new(text_callback=self$handle_text,
                                        message_callback=self$handle_message,
                                        warning_callback=self$handle_warning,
                                        error_callback=self$handle_error,
                                        value_callback=self$handle_value,
                                        graphics_callback=self$handle_graphics,
                                        envir=self$envir,
                                        enclos=list(
                                            display=self$display
                                        ))
        },

        do = function(...) self$context$do(...),
        eval = function(expr) self$context$eval(expr),
        evaluate = function(expressions) self$context$evaluate(expressions),

        handle_text = function(text) {
            text <- paste(text,collapse="\n")
            self$stream(text = text,
                        stream = "stdout")
        },

        last_plot_id = character(0),
        handle_graphics = function(plt,update=FALSE) {

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
                    text <- paste(text,collapse="\n")
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
        stream = function(text,stream_name) {
            # log_out("OutputWidget$stream")
            self$outputs <- append(self$outputs,
                                   list(list(
                                       output_type = "stream",
                                       name = stream_name,
                                       text = text
                                   )))
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
                }
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
with.OutputWidget <- function(data,expr,...) data$context$eval(substitute(expr))
