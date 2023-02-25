#' Widgets to receive output
#' @description Classes and constructors to wrap output created by code
#' @importFrom uuid UUIDgenerate
#' @include widget-dom.R
#' @name OutputWidget

#' @rdname OutputWidget
#' @export
OutputWidgetClass <- R6Class_("OutputWidget",
    inherit = DOMWidgetClass,
    public = list(
        #' @field _view_name Name of the Javascript model view in the frontend
        `_view_name` = structure(Unicode("OutputView"),sync=TRUE),
        #' @field _model_name Name of the Javascript model in the frontend
        `_model_name` = structure(Unicode("OutputModel"),sync=TRUE),
        #' @field _view_module Name of the module where the view is defined
        `_view_module` = structure(Unicode("@jupyter-widgets/output"),sync=TRUE),
        #' @field _model_module Name of the Javascript module with the model
        `_model_module` = structure(Unicode("@jupyter-widgets/output"),sync=TRUE),
        #' @field _view_module_version Version of the module where the view is defined
        `_view_module_version` = structure(Unicode(jupyter_widgets_output_version),sync=TRUE),
        #' @field _model_module_version Version of the module where the model is defined
        `_model_module_version` = structure(Unicode(jupyter_widgets_output_version),sync=TRUE),
        #' @field msg_id Unicode string with the id of the last message sent to the frontend.
        msg_id = structure(Unicode(""),sync=TRUE),
        #' @description Initializing function
        #' @param append_output Logical, whether existing output should be appended to or overwritten.
        #' @param envir An environment, where expressions are evaluated.
        #' @param use_display Logical, whether the display mechanism is used internally for 
        #'    output streams.
        #' @param ... Any other arguments, passed to the superclass initializer.
        initialize = function(append_output = TRUE,
                              envir = new.env(),
                              use_display = FALSE,
                              ...){
            super$initialize(...)
            private$envir <- envir
            context <- Context$new(envir=private$envir,
                                   attachment=list(
                                       display=private$display
                                   ))
            context$on_enter(private$enter)
            context$on_exit(private$exit)

            context$on_eval(private$handle_eval)
            context$on_result(private$handle_result)
            context$on_message(private$handle_message)
            context$on_warning(private$handle_warning)
            context$on_error(private$handle_error)

            context$on_print(private$handle_graphics,exit=private$handle_text)
            context$on_cat(private$handle_graphics,exit=private$handle_text)
            
            private$graphics <- GraphicsDevice$new()

            self$context <- context
            private$append_output <- append_output
            private$use_display <- use_display
            private$kernel <- get_current_kernel()
        },
        #' @description 
        #' Evaluate one or several expresions
        #' @param ... A single expression or several expressions
        #'   included in curly braces.
        do = function(...) self$context$do(...),
        #' @description
        #' Evaluate a single expression
        #' @param expr A single expression.
        eval = function(expr,...) self$context$eval(expr,...),
        #' @description
        #' Evaluate a single expression
        #' @param expressions A list of expressions.
        evaluate = function(expressions,...) self$context$evaluate(expressions,...),
        #' @description A variant of \code{\link{display}} for output within a display widget.
        display = function(...){
            d <- display_data(...)
            private$display_send(d)
        },
        #' @description 
        #' Show textual output in the widget area, see \code{\link{cat}}.
        cat = function(...,sep=" "){
            text <- paste(...,sep=sep)
            private$stream(text,"stdout")
        },
        #' @description 
        #' Show printed output in the widget area, see \code{\link{print}}
        print = function(x,...){
            k <- get_current_kernel()
            text <- capture.output(k$print(x,...))
            private$stream(text,"stdout")
        },
        #' @description Clear the output
        #' @param wait Logical, whether to wait for the frontend to clear
        #'   the output.
        clear_output = function(wait=FALSE){
            private$sync_suspended <- TRUE
            private$outputs <- list()
            # kernel <- get_current_kernel()
            # kernel$clear_output(wait=wait)
            private$sync_suspended <- FALSE
            private$send_state("outputs")
        },
        #' @field context NULL or (after initialization) an object inclass
        #'    "Context" -- see \code{\link{Context}}.
        context = NULL

    ),
    private = list(
        #' @field graphics A graphics object, used to control graphical output.
        graphics = NULL,
        #' @field envir An environment, within whicht expressions are evaluated.
        envir = NULL,
        #' @append_output A logiccal value, whether output is appended to or overwritten.
        append_output = TRUE,
        #' @use_display A logiccal value, whether .
        use_display = FALSE,

        enter = function(){
            parent <- private$kernel$get_parent("shell")
            private$msg_id <- parent$header$msg_id
            private$graphics$activate()
            # log_out(sprintf("msg_id set to '%s'",private$msg_id))
        },
        exit = function(){
            private$msg_id <- ""
            private$graphics$suspend()
            # log_out(sprintf("msg_id set to '%s'",private$msg_id))
        },

        handle_eval = function() {
            # log_out("Widget-context: handle_eval")
            private$handle_text()
            private$handle_graphics()
        },
        
        handle_text = function() {
            # log_out("Widget-context: handle_text")
            text <- self$context$get_text()
            text <- paste(text,collapse="\n")
            private$stream(text = text,
                        stream = "stdout")
        },
        
        last_plot_id = character(0),
        current_plot = NULL,
        last_plot = NULL,

        handle_graphics = function() {
            # log_out("Widget-context: handle_graphics")

            if(!private$graphics$is_active()) return(NULL)
            plt <- private$graphics$get_plot()

            if(!length(plt) || !private$graphics$complete_page()) return(NULL)
            new_page <- private$graphics$new_page(reset=TRUE)
            private$last_plot <- private$current_plot
            if(new_page || plot_has_changed(current=plt,last=private$last_plot)) 
                private$current_plot <- plt
            else return(NULL)

            update <- !new_page

            if(update){
                id <- private$last_plot_id
            } 
            else {
                id <- UUIDgenerate()
                private$last_plot_id <- id
            } 
            # log_out(sprintf("OutputWidget$handle_graphics(...,update=%s)",if(update)"TRUE"else"FALSE"))

            width      <- getOption("jupyter.plot.width",6)
            height     <- getOption("jupyter.plot.height",6)
            pointsize  <- getOption("jupyter.plot.pointsize",12)
            resolution <- getOption("jupyter.plot.res",150)
            scale      <- getOption("jupyter.plot.scale",0.5)
            units      <- getOption("jupyter.plot.units","units")

            # log_out("OutputWidget$graphics_send")
            # log_out("  -- update = ",if(update)"TRUE"else"FALSE")
            # log_out("  -- id = ",private$last_plot_id)
            # 
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
            private$display_send(d)
        },

        handle_message = function(m) {
            text <- conditionMessage(m)
            text <- paste(text,collapse="\n")
            private$stream(text = text,
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
            log_warning(text)
            private$stream(text = text,
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
            log_error(text)
            private$stream(text = text,
                        stream = "stderr")
        },

        handle_result = function(x,visible) {
            if(visible){
                if(any(class(x) %in% getOption("rkernel_displayed_classes"))){
                    d <- display_data(x)
                    private$display_send(d)
                }
                else if(inherits(x,"display_data")){
                    private$display_send(x)
                }
                else if(inherits(x,"update_display_data")){
                    private$display_send(x)
                }
                # 'print()'ing does not work well with 
                # classic Jupyter notebooks - but with JupyterLab and 
                # Voila seems to work well.
                else {
                    d <- display_data(x)
                    d$data <- d$data["text/plain"]
                    private$display_send(d)
                }
                # else {
                #     text <- capture.output(print(x))
                #     text <- paste(c("",text),collapse="\n")
                #     private$stream(text = text,
                #                 stream = "stdout")
                # }
            }
        },
        stdout = function(text) private$stream(text,"stdout"),
        stderr = function(text) private$stream(text,"stderr"),

        outputs = structure(List(),sync=TRUE),
        current_output = NULL,
        stream = function(text,stream_name) {
            if(!nzchar(text)) return()
            private$sync_suspended <- TRUE
            if(private$use_display && stream_name=="stdout"){
                d <- display_data(`text/plain`=text)
                private$current_output <- list(
                    output_type = "display_data",
                    data = d$data,
                    metadata = d$metadata
                )
                if(private$append_output){
                    l <- length(private$outputs)
                    private$outputs[[l+1]] <- private$current_output
                }
                else {
                    private$outputs[[1]] <- private$current_output
                }
            }
            else {
            # log_out("Widget-context: stream")
                if(private$append_output){
                    # log_out(private$outputs,use.str=TRUE)
                    l <- length(private$outputs)
                    if(!is.null(private$current_output) &&
                       identical(private$current_output$output_type, "stream") &&
                       identical(private$current_output$name, stream_name) && l > 0){
                        private$current_output$text <- paste0(private$current_output$text,
                                                           text)
                        private$outputs[[l]] <- private$current_output
                    }
                    else {
                        private$current_output <- list(
                            output_type = "stream",
                            name = stream_name,
                            text = text
                        )
                        outputs <- private$outputs
                        outputs[[l+1]] <- private$current_output
                        private$outputs <- outputs
                    }
                # kernel <- get_current_kernel()
                # kernel$stream(text,stream_name)
                }
                else {
                    private$current_output <- list(
                        output_type = "stream",
                        name = stream_name,
                        text = text
                    )
                # log_out("\n\n================================================================\n")
                #private$traits$outputs$set(list(private$current_output),notify=FALSE)
                #private$send_state()
                    private$outputs[[1]] <- private$current_output
                }
            }
            private$sync_suspended <- FALSE
            private$send_state("outputs")
        },
        display_index = integer(0),
        display_send = function(d){
            if(!(class(d)%in%c("display_data","update_display_data")))
                stop("'display_data' or 'update_display_data' object required")
            out_data <- list(output_type = "display_data",
                             data = d$data,
                             metadata = d$metadata)
            # log_out("Widget-context: display_send")
            if(private$append_output){
                id <- d$transient$display_id
                update <- inherits(d,"update_display_data")
                l <- length(private$display_index)
                # log_out(id,use.print=TRUE)
                # log_out("OutputWidget$display_send")
                # log_out(" -- id = ",id)
                # log_out(private$display_index,use.print=TRUE)
                if(update){
                    if(id == "last" && l > 0){
                        i <- private$display_index[l]
                        private$outputs[[i]] <- out_data
                    }
                    if(id %in% names(private$display_index)){
                        i <- private$display_index[id]
                        private$outputs[[i]] <- out_data
                    }
                } else {
                    if(l < 1 || !(id %in% names(private$display_index))){
                        i <- length(private$outputs) + 1L
                        ii <- l + 1L
                        private$display_index[ii] <- i
                        names(private$display_index)[ii] <- id
                        private$outputs[[i]] <- out_data
                        private$current_output <- out_data
                    }
                }
            }
            else {
                private$outputs <- list(out_data)
                private$current_output <- out_data
                #private$send_state()
            }
        },
        last_display = function(){
            l <- length(private$display_index)
            if(l > 0){
                names(private$display_index[l])
            }
            else
                character(0)
        },
        clear = function(){
            private$display_index <- integer(0)
            private$outputs <- list()
        },
        kernel=NULL
    )
)

#' @rdname OutputWidget
#' @param append_output Logical value, whether new output is appended to existing
#'    output in the widget or the output is overwritten
#' @param ... Other arguments, ignored.
#' @export
OutputWidget <- function(append_output=FALSE,...) 
                   OutputWidgetClass$new(append_output=append_output,
                                         ...)


#' @rdname OutputWidget
#' @param data An "OutputWidget" object
#' @param expr An expression to evaluate, or a sequence of expression, 
#'    encapsulated by curly braces.
#' @param enclos An enclosing environment.
#' @export
with.OutputWidget <- function(data,expr,envir=list(),enclos=parent.frame(),...)
    data$context$eval(substitute(expr),envir=envir,enclos=enclos)
