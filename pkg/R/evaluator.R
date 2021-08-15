#' @importFrom evaluate evaluate new_output_handler
#' @importFrom utils capture.output
#' @importFrom svglite svgstring
#' @export
Evaluator <- R6Class("Evaluator",
    public = list(
 
        nframes = -1,
        output_handlers = list(),
        aborted = FALSE,
        status = "ok",
        payload = list(),
        results = list(),
        env = list(),

        startup = function(...) {

            self$env <- new.env()
            attach(self$env,name="RKernel")
            pos <- match("RKernel",search())
            assign("q",self$quit,pos=pos)
            assign("quit",self$quit,pos=pos)

            assign("display",display,pos=pos)
            assign("Page",Page,pos=pos)

            options(device=svg)
            options(pager=self$pager)
            options(crayon.enabled=TRUE,crayon.colors=256L)

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

        eval = function(code,...,silent=FALSE){

            options(device=svg)
            options(pager=self$pager)

            add_paged_classes(c("help_files_with_topic","packageIQR"))

            if(self$nframes < 0){
                getnframes <- function(e) self$nframes <- sys.nframe()
                tryCatch(evaluate(
                    'stop()',
                    stop_on_error = 1L,
                    output_handler = new_output_handler(error = getnframes)))
            }

            self$results <- list()
            if(self$aborted) return(self$results)
            if(silent)
                output_handler <- self$output_handler$silent
            else
                output_handler <- self$output_handler$default

            expr <- try(parse(text=code),silent=TRUE)
            if(inherits(expr,"try-error")){
                condition <- attr(expr,"condition")
                result <- list(
                    text = condition$message,
                    stream = "stderr")
                self$add_result(result)
                self$status <- "error"
            }
            else {
                tryCatch(
                    evaluate(code,
                             envir=.GlobalEnv,
                             stop_on_error=1L,
                             output_handler=self$output_handlers$default,
                             new_device=TRUE),
                    interrupt = self$handle_interrupt)
            }
            return(self$results)
        },

        add_result = function(result){
            self$results <- append(self$results,list(result))
        },

        add_payload = function(payload){
            self$payload <- append(self$payload,list(payload))
        },

        get_payload = function(clear=FALSE){
            payload <- self$payload
            if(clear)
                self$payload <- list()
            return(payload)
        },

        get_status = function(reset=FALSE){
            status <- self$status
            if(reset)
                self$status <- "ok"
            return(status)
        },

        is_aborted = function(reset=FALSE){
            aborted <- self$aborted
            if(reset)
                self$aborted <- FALSE
            return(aborted)
        },

        quit = function(...){
            payload <- list(source="ask_exit",
                            keepkernel=FALSE)
            self$add_payload(payload)
        },

        pager = function(files,header,title,delete.file){
            text <- c(header,"",title,"")
            for(file in files){
                text <- c(text,readLines(file))
            }
            if(delete.file){
                file.remove(files)
            }
            text <- paste(text,collapse="\n")
            self$page("text/plain"=text)
        },

        handle_text = function(text) {
            # cat("handle_text")
            result <- list(
                stream = "stdout",
                text   = text,
                status = "ok"
            )
            self$add_result(result)
        },

        handle_graphics = function(plt) {
            # cat("handle_graphics")
            width <- getOption("jupyter.plot.width",6)
            height <- getOption("jupyter.plot.height",6)
            embedded <- getOption("jupyter.embed.graphics",TRUE)
            s <- svgstring(width=width,height=height,standalone=FALSE)
            replayPlot(plt)
            dev.off()
            svgstr <- s()
            if(embedded){
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
                self$add_result(result)
            } else {
                payload <- list(
                    source="page",
                    data=list(
                        "text/html"=unclass(svgstr)
                    ),
                    start=1
                )
                self$add_payload(payload)
            }
        },

        handle_message = function(m) {
            text <- conditionMessage(m)
            result <- list(
                stream = "stdout",
                text   = text
            )
            self$add_result(result)
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
                text   = text
            )
            self$add_result(result)
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
                text   = text
            )
            self$status <- "error"
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
                result$error <- list(
                                name = "ERROR",
                                value = text,
                                traceback = list(traceback))
                self$aborted <- TRUE
            }
            self$add_result(result)
        },

        handle_value = function(x,visible) {
            result <- NULL
            if(visible){
                if(any(class(x) %in% getOption("rkernel_paged_classes"))){
                    displayed <- display(x)
                    payload <- list(source="page",
                                    data=displayed$data,
                                    start=1)
                    self$add_payload(payload)
                }
                else if(any(class(x) %in% getOption("rkernel_displayed_classes"))){
                    displayed <- display(x)
                    result <- list(display_data=unclass(displayed))
                }
                else if(inherits(x,"clear_output")){
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
                    self$add_payload(unclass(x))
                }
                else {
                    text <- capture.output(print(x))
                    text <- paste(text,collapse="\n")
                    result <- list(stream = "stdout",
                                   text   = text)
                }
                self$set_last_value(x)
            }
            if(length(result))
                self$add_result(result)
        },

        handle_interrupt = function(i){
            result <- list(
                stream = "stderr",
                text   = "<interrupted>"
            )
            self$add_result(result)
            seff$status <- "aborted"
            self$aborted <- TRUE
        },

        set_last_value = function(x){
            pos <- match("RKernel",search())
            assign(".Last.value",x,pos=pos)
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

