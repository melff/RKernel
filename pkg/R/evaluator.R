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
                eval_with_vis <- withVisible(try(eval(expr,.GlobalEnv)))
                eval_result <- eval_with_vis$value
                if(inherits(eval_result,"try-error")){
                    condition <- attr(eval_result,"condition")
                    result <- list(
                        text = condition$message,
                        stream = "stderr",
                        status = "error")
                }
                else{
                    if(eval_with_vis$visible){
                        eval_output <- capture.output(print(eval_result))
                        result <- list(
                            text = paste(eval_output,collapse="\n"),
                            stream = "stdout",
                            status = "ok")
                    }
                    else result <- list(status = "ok")
                    payload <- attr(eval_result,"payload")
                    result$payload <- prep_payload(payload)
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
