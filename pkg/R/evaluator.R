#' @importFrom evaluate evaluate new_output_handler
#' @importFrom utils capture.output
#' @export
Evaluator <- R6Class("Evaluator",
    public = list(
        results = list(),
        
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
                             new_device=FALSE),
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
        handle_graphics = function(pltObj) {},
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
                result <- c(result,
                            list(
                                ename = "ERROR",
                                evalue = eval_result$message,
                                traceback = list("<execution suspended>"),
                                abort = TRUE))
            }
            self$results <- c(self$results,list(result))
        },
        handle_value = function(x,visible) {
            # cat("handle_value")
            result <- list(status = "ok")
            if(visible){
                #cat("handle_visible_value")
                text <- capture.output(print(x))
                text <- paste(text,collapse="\n")
                result <- c(result,
                            list(
                                stream = "stdout",
                                text   = text))
            }
            result$payload <- attr(x,"payload")
            if(inherits(x,"display_data"))
                result$display_data <- x
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

# identity <- function(x) x


prep_payload <- function(payload){
    names(payload) <- NULL
    payload[sapply(payload,has_source)]
}

has_source <- function(x) "source" %in% names(x)

ask_exit_obj <- structure(TRUE,
                          payload=list(
                              list(source="ask_exit",
                                   keepkernel=FALSE)))

q_evaluator <- function(...) invisible(ask_exit_obj)
