#' @export
Evaluator <- R6Class("Evaluator",
    public = list(

        startup = function(...) {
            env <- new.env()
            attach(env,name="pseudo-package:RKernel")
            assign("q",q_evaluator,pos=2L)
        },

        eval = function(code,...){
            expr <- try(parse(text=code),silent=TRUE)
            if(inherits(expr,"try-error")){
                condition <- attr(expr,"condition")
                result <- list(
                    text = condition$message,
                    stream = "stderr",
                    status = "error")
            }
            else {
                eval_output <- capture.output(eval_with_vis <- withVisible(
                                                  tryCatch(eval(expr,.GlobalEnv),
                                                           error = identity,
                                                           warning = identity,
                                                           message = identity,
                                                           interrupt = identity)
                                              ))
                eval_result <- eval_with_vis$value
                result <- list(status = "ok")
                if(length(eval_result)){
                    if(inherits(eval_result,"condition")) str(eval_result)
                    if(inherits(eval_result,"error")){
                        stop_on_error <- getOption("rkernel_stop_on_error",FALSE)
                        result$status <- "error"
                        result$stream <- "stderr"
                        result$text <- paste("ERROR:",eval_result$message)
                        if(isTRUE(stop_on_error)){
                            result <- c(result,list(
                                ename = "ERROR",
                                evalue = eval_result$message,
                                traceback = list("<execution suspended>"),
                                abort = TRUE))
                        }
                    }
                    else if(inherits(eval_result,"warning")){
                        result$stream <- "stderr"
                        result$text <- paste("Warning:",eval_result$message)
                    }
                    else if(inherits(eval_result,"message")){
                        result$stream <- "stdout"
                        result$text <- eval_result$message
                    }
                    else if(inherits(eval_result,"interrupt")){
                        result$stream <- "stderr"
                        result$text <- "<interrupted>"
                    }
                    else if(eval_with_vis$visible){
                            eval_output <- c(eval_output,
                                             capture.output(print(eval_result)))
                    }
                    payload <- attr(eval_result,"payload")
                    result$payload <- prep_payload(payload)
                } 
                if(length(eval_output)){
                    result$text <- paste(eval_output,collapse="\n")
                    result$stream = "stdout"
                }
            }
            return(result)
        }
))

identity <- function(x) x


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
