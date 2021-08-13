#' @importFrom evaluate evaluate new_output_handler
#' @importFrom utils capture.output
#' @importFrom svglite svgstring
#' @export
Evaluator <- R6Class("Evaluator",
    public = list(
        results = list(),

        nframes = -1,
        
        output_handlers = list(),

        startup = function(...) {
            env <- new.env()
            attach(env,name="RKernel")
            assign("q",q_evaluator,pos=2L)

            options(device=svg)

            self$output_handlers$default <- new_output_handler(
                text     = self$handle_text,
                graphics = self$handle_graphics,
                message  = self$handle_message,
                warning  = self$handle_warning,
                error    = self$handle_error,
                value    = self$handle_value)

            self$output_handlers$silent <- new_output_handler(
                text     = identity,
                graphics = identity,
                message  = identity,
                warning  = identity,
                error    = identity,
                value    = identity)

        },

        eval = function(code,...){

            if(self$nframes < 0){
                getnframes <- function(e) self$nframes <- sys.nframe()
                tryCatch(evaluate(
                    'stop()',
                    stop_on_error = 1L,
                    output_handler = new_output_handler(error = getnframes)))
            }

            self$results <- list()

            expr <- try(parse(text=code),silent=TRUE)
            if(inherits(expr,"try-error")){
                condition <- attr(expr,"condition")
                result <- list(
                    text = condition$message,
                    stream = "stderr",
                    status = "error")
                self$results <- append(self$results,list(result))
            }
            else {
                eval_results <- tryCatch(
                    evaluate(code,
                             envir=.GlobalEnv,
                             stop_on_error=1L,
                             output_handler=self$output_handlers$default,
                             new_device=TRUE),
                    interrupt = self$handle_interrupt)
            }
            return(self$results)
        },

        handle_text = function(text) {
            # cat("handle_text")
            result <- list(
                stream = "stdout",
                text   = text,
                status = "ok"
            )
            self$results <- c(self$results,list(result))
        },

        handle_graphics = function(plt) {
            # cat("handle_graphics")
            width <- getOption("jupyter.plot.width",6)
            height <- getOption("jupyter.plot.height",6)
            s <- svgstring(width=width,height=height,standalone=FALSE)
            replayPlot(plt)
            dev.off()
            svgstr <- s()
            result <- list(
                display_data = list(
                    data = list(
                        "image/svg+xml"=unclass(svgstr)
                    ),
                    metadata = list(
                        "image/svg+xml"=list(
                            width=width,
                            height=height
                        )
                    )
                )
            )
            self$results <- c(self$results,list(result))
        },

        handle_message = function(m) {
            text <- conditionMessage(m)
            result <- list(
                stream = "stdout",
                text   = text,
                status = "ok"
            )
            self$results <- c(self$results,list(result))
        },
        handle_warning = function(w) {
            text <- conditionMessage(w)
            call <- conditionCall(w)
            if(is.null(call)) {
                text <- paste0("Warning: ",text)
            } else {
                call <- deparse(call)[[1]]
                text <- paste0("Warning in ",call,": ",text)
            }
            result <- list(
                stream = "stderr",
                text   = text,
                status = "ok"
            )
            self$results <- c(self$results,list(result))
        },
        handle_error = function(e) {
            text <- conditionMessage(e)
            call <- conditionCall(e)
            if(is.null(call)) {
                text <- paste0("Error: ",text)
            } else {
                call <- deparse(call)[[1]]
                text <- paste0("Error in ",call,": ",text)
            }
            result <- list(
                stream = "stderr",
                text   = text,
                status = "error"
            )
            stop_on_error <- getOption("rkernel_stop_on_error",FALSE)
            if(isTRUE(stop_on_error)){
                calls <- sys.calls()
                calls <- head(calls,-3)
                drop_prev <- self$nframes - 2
                calls <- tail(calls,-drop_prev)
                calls <- limitedLabels(calls)
                if(length(calls))
                    calls <- paste0(format(seq_along(calls),justify="right"),
                                    ". ",
                                    calls)
                traceback <- c("\nTraceback:",calls)
                traceback <- paste(traceback,collapse="\n")
                result <- c(result,
                            list(
                                ename = "ERROR",
                                evalue = text,
                                traceback = list(traceback),
                                abort = TRUE))
            }
            self$results <- c(self$results,list(result))
        },
        handle_value = function(x,visible) {
            result <- NULL
            if(visible){
                if(inherits(x,"clear_output")){
                    result <- list(clear_output=unclass(x))
                }
                else if(inherits(x,"execute_result")){
                    result <- list(execute_result=unclass(x))
                }
                else if(inherits(x,"display_data")){
                    result <- list(display_data=unclass(x))
                }
                else if(inherits(x,"update_display_data")){
                    result <- list(update_display_data=unclass(x))
                }
                else if(inherits(x,"payload")){
                    result <- list(payload=unclass(x))
                }
                else {
                    text <- capture.output(print(x))
                    text <- paste(text,collapse="\n")
                    result <- list(stream = "stdout",
                                   text   = text,
                                   status = "ok")
                }
            }
            if(length(result))
                self$results <- c(self$results,list(result))
        },
        handle_interrupt = function(i){
            result <- list(
                stream = "stderr",
                text   = "<interrupted>",
                status = "aborted"
            )
            self$results <- c(self$results,list(result))
        }
))

result_class <- function(x,cl){
    if(!is.list(x) || !inherits(x,cl)) return(FALSE)
    if(cl %in% c("execute_result","display_data","update_display_data"))
        return(check_names(x,
                           mandatory=c("data","metadata"),
                           optional="transient"))
    else if(cl == "clear_output")
        return(check_names(x,mandatory="wait"))
    else if(cl == "payload")
        return(check_names(x,
                           mandatory="source",
                           any_other=TRUE))
    else FALSE
}

check_names <- function(x,mandatory,optional=NULL,any_other=FALSE){
    if(!all(mandatory%in%names(x))) return(FALSE)
    else if(!any_other && !all(names(x)%in%c(mandatory,optional))) return(FALSE)
    else return(TRUE)
}

ask_exit_obj <- structure(list(source="ask_exit",
                               keepkernel=FALSE),
                          class="payload")

q_evaluator <- function(...) invisible(ask_exit_obj)
