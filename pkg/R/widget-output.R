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
        `_view_module_version` = structure(Unicode(jupyter_widgets_output_version()),sync=TRUE),
        #' @field _model_module_version Version of the module where the model is defined
        `_model_module_version` = structure(Unicode(jupyter_widgets_output_version()),sync=TRUE),
        #' @field msg_id Unicode string with the id of the last message sent to the frontend.
        msg_id = structure(Unicode(""),sync=TRUE),
        #' @field outputs A list with output strings
        outputs = structure(List(),sync=TRUE),
        #' @description Initializing function
        #' @param append_output Logical, whether existing output should be appended to or overwritten.
        #' @param envir An environment, where expressions are evaluated.
        #' @param use_display Logical, whether the display mechanism is used internally for 
        #'    output streams.
        #' @param ... Any other arguments, passed to the superclass initializer.
        initialize = function(append_output = TRUE,
                              ...){
            super$initialize(...)
            private$append_output <- append_output
        },
        #' @description A variant of \code{\link{display}} for output within a display widget.
        #' @param ... Further arguments, passed on to the 'evaluate' method of the \code{\link{Context}}
        #'            class,
        display = function(...){
            d <- display_data(...)
            private$display_send(d)
        },
        #' @description Clear the output
        #' @param wait Logical, whether to wait for the frontend to clear
        #'   the output.
        clear = function(wait=FALSE){
            private$sync_suspended <- TRUE
            self$outputs <- list()
            # kernel <- get_current_kernel()
            # kernel$clear_output(wait=wait)
            private$sync_suspended <- FALSE
            self$send_state("outputs")
        },
        stdout = function(text) private$stream(text,"stdout"),
        stderr = function(text) private$stream(text,"stderr")
    ),
    private = list(

        append_output = FALSE,
        current_output = NULL,
        r_msg_incomplete = FALSE,
        r_msg_frag = "",

        stream = function(text,stream_name){
            # log_out("Widget-context: stream")
            text <- split_string1(text, DLE)
            for(chunk in text){
                if (!length(chunk) || !nzchar(chunk)) next
                if (startsWith(chunk, MSG_BEGIN)) {
                    if (endsWith(chunk, MSG_END)) {
                        msg <- remove_prefix(chunk, MSG_BEGIN) |> remove_suffix(MSG_END)
                        msg <- msg_unwrap(msg)
                        private$handle_r_msg(msg)
                    } else {
                        private$r_msg_incomplete <- TRUE
                        private$r_msg_frag <- remove_prefix(chunk, MSG_BEGIN)
                    }
                }
                else if(endsWith(chunk, MSG_END)){
                    msg <- paste0(private$r_msg_frag, remove_suffix(chunk, MSG_END))
                    private$r_msg_incomplete <- FALSE
                    private$r_msg_frag <- ""
                    msg <- msg_unwrap(msg)
                    private$handle_r_msg(msg)
                }
                else {
                    if(private$r_msg_incomplete) {
                        private$r_msg_frag <- paste0(private$r_msg_frag, chunk)
                    }
                    else if(nzchar(chunk)) {
                        private$stream1(chunk, stream_name)
                    }
                }
            }
        },
        stream1 = function(text,stream_name) {
            if(!nzchar(text)) return()
            # log_out("Widget-context: stream1")
            private$sync_suspended <- TRUE
            l <- length(self$outputs)
            if(private$append_output){
                if(!is.null(private$current_output) &&
                   identical(private$current_output$output_type, "stream") &&
                   identical(private$current_output$name, stream_name) && l > 0){
                    private$current_output$text <- paste0(private$current_output$text,
                                                          text)
                    self$outputs[[l]] <- private$current_output
                }
                else {
                    private$current_output <- list(
                        output_type = "stream",
                        name = stream_name,
                        text = text
                    )
                    outputs <- self$outputs
                    outputs[[l+1]] <- private$current_output
                    self$outputs <- outputs
                }
            }
            else {
                private$current_output <- list(
                    output_type = "stream",
                    name = stream_name,
                    text = text
                )
                self$outputs <- list(private$current_output)
            }
            private$sync_suspended <- FALSE
            self$send_state("outputs")
        },
        handle_r_msg = function(msg) {
            if(!is.list(msg)) return(NULL)
            if (msg$type %in% c("display_data", "update_display_data")) {
                d <- structure(msg$content, class = msg$type)
                private$display_send(d)
            }
            else {
                msg_send(msg) # Pass message on to frontend
            }
        },
        display_index = integer(0),
        display_send = function(d){
            if(!inherits(d,"display_data") && !inherits(d,"update_display_data"))
                stop("'display_data' or 'update_display_data' object required")
            out_data <- list(output_type = "display_data",
                             data = d$data,
                             metadata = d$metadata)
            # log_out("Widget-context: display_send")
            id <- d$transient$display_id
            update <- inherits(d,"update_display_data")
            l <- length(private$display_index)
            # log_out(id,use.print=TRUE)
            # log_out("OutputWidget$display_send")
            # log_out(" -- id = ", id)
            # log_out(class(d))
            # log_out(private$display_index,use.print=TRUE)
            if(private$append_output){
                if(update){
                    if(id == "last" && l > 0){
                        i <- private$display_index[l]
                        self$outputs[[i]] <- out_data
                    }
                    if(id %in% names(private$display_index)){
                        i <- private$display_index[id]
                        self$outputs[[i]] <- out_data
                    } else {
                        # log_out(private$display_index, use.print = TRUE)
                        log_error(paste("Display with id", id, "not found."))
                    }
                } else {
                    i <- length(self$outputs) + 1L
                    ii <- l + 1L
                    private$display_index[ii] <- i
                    names(private$display_index)[ii] <- id
                    self$outputs[[i]] <- out_data
                    private$current_output <- out_data
                }
            } else {
                private$display_index <- structure(1,names=id)
                self$outputs <- list(out_data)
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
        clear_output = function(){
            private$display_index <- integer(0)
            self$outputs <- list()
        }
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
#' @param ... Other arguments, ignored.
#' @export
with.OutputWidget <- function(data,expr,envir=list(),enclos=parent.frame(),clear=TRUE,...){
    sout <- NULL
    serr <- NULL
    sout_con <- textConnection("sout","w",local=TRUE)
    serr_con <- textConnection("serr","w",local=TRUE)
    sink(serr_con,type="message")
    sink(sout_con,type="output")
    r <- eval(substitute(withVisible(expr)),envir=envir,enclos=enclos)
    # cat("")
    if(r$visible)
        print(r$value)
    sink(type="message")
    sink(type="output")
    close(serr_con)
    close(sout_con)
    data$clear()
    if(length(sout)){
        sout <- paste(sout,collapse="\n")
        data$stdout(sout)
    }
    if (length(serr)) {
        serr <- paste(serr, collapse = "\n")
        data$stderr(serr)
    }
}
