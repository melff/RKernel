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
                eval_output <- capture.output(eval_with_vis <- withVisible(try(eval(expr,.GlobalEnv),silent=TRUE)))
                eval_result <- eval_with_vis$value
                result <- list(status = "ok")
                if(length(eval_result)){
                    if(inherits(eval_result,"try-error")){
                        condition <- attr(eval_result,"condition")
                        error.critical <- getOption("rkernel.error.critical",FALSE)
                        result$status <- "error"
                        result$stream <- "stderr"
                        if(isTRUE(error.critical)){
                            result <- c(result,list(
                                ename = class(condition)[1],
                                evalue = condition$message,
                                traceback = list(),
                                abort = TRUE))
                        }
                        else {
                            result$text <- condition$message
                        }    
                    }
                    else if(eval_with_vis$visible){
                            eval_output <- c(eval_output,capture.output(print(eval_result)))
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
