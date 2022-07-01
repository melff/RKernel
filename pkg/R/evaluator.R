#' @importFrom utils capture.output
#' @importFrom svglite svgstring
#' @importFrom grDevices png

check_help_port <- function(port){
    url <- sprintf("http://127.0.0.1:%d/doc/html/index.html",port)
    res <- try(curlGetHeaders(url))
    if(inherits(res,"try-error")) return(FALSE)
    if(attr(res,"status") == 200) return(TRUE)
    return(FALSE)
}

#' The Evaluator Class
#'
#' @description An object of this class provides the context in which
#'   Jupyter notebook cells are run -- i.e. the expression within are
#'   evaluated. Usually there is only one of such objects needed.
#' @export
Evaluator <- R6Class("Evaluator",
    public = list(
        #' @description
        #' Initialize the object
        #' @param kernel The kernel to which this evaluator belongs
        initialize = function(kernel){
            private$kernel <- kernel
        },
        #' @description
        #' Prepare the object for evaluating expressions
        #' @param ... Any kind of arguments, ignored.
        startup = function(...) {

            private$env <- new.env()
            #attach(private$env,name="RKernel")
            #pos <- match("RKernel",search())
            assign("q",self$quit,envir=private$env)
            assign("quit",self$quit,envir=private$env)
            assign("interactive",function()TRUE,envir=private$env)
            # 
            # assign("display",self$display,envir=private$env)
            # assign("display",display,envir=private$env)
            # assign("stream",self$stream,pos=pos)
            # #assign("cat",private$cat,pos=pos)
            # 
            # assign("Javascript",Javascript,pos=pos)
            # assign("Math",LaTeXMath,pos=pos)
            # assign("raw_html",raw_html,pos=pos)
            # assign("Page",Page,pos=pos)
            assign("View",View,envir=private$env)
            # assign("ls_str",ls_str,pos=pos)
            # 
            # assign("add_paged_classes",add_paged_classes,pos=pos)
            # assign("add_displayed_classes",add_displayed_classes,pos=pos)
            # 
            # assign("comm_manager",self$comm_manager,pos=pos)
            # assign("help_port",self$help_port,pos=pos)
            # 
            # if("var_dic_list" %in% objects(envir=.GlobalEnv)) 
            #     rm(var_dic_list,envir=.GlobalEnv)
            # assign("var_dic_list",self$var_dic_list,pos=pos)

            options(pager=self$pager,
                    crayon.enabled=TRUE,crayon.colors=256L,
                    jupyter.graphics.types=c("image/png","application/pdf"),
                    jupyter.plot.width=6,
                    jupyter.plot.height=6,
                    jupyter.plot.pointsize=12,
                    jupyter.plot.res=150,
                    jupyter.plot.units="in",
                    jupyter.plot.scale=0.5,
                    jupyter.update.graphics=TRUE,
                    repos=c(CRAN="https://cloud.r-project.org"),
                    rkernel_stop_on_error=TRUE)


            add_displayed_classes(c("help_files_with_topic","packageIQR","hsearch"))
            add_displayed_classes(c("htmlwidget","html_elem","shiny.tag"))

            private$comm_dispatcher <- private$kernel$comm_dispatcher

            private$context <- Context$new(text_callback=private$handle_text,
                                        message_callback=private$handle_message,
                                        warning_callback=private$handle_warning,
                                        error_callback=private$handle_error,
                                        value_callback=private$handle_value,
                                        graphics_callback=private$handle_graphics,
                                        envir=.GlobalEnv,
                                        attachment=private$env)

            suppressMessages(trace(example,tracer=quote(if(missing(run.donttest)) run.donttest<-TRUE),print=FALSE))
            home_dir <- Sys.getenv("HOME")
            jupyter_config <- file.path(home_dir,".jupyter","RKernel-config.R")
            if(file.exists(jupyter_config)) 
                source(jupyter_config)
            if(file.exists("RKernel-startup.R"))
                source("RKernel-startup.R")

            private$start_help_system()
            assign("help.start",help.start,envir=private$env)
        },
        #' @description
        #' Shut the session down
        shutdown = function(){
            base::q()
        },
        #' @description
        #' Evaluate R code
        #' @param code A string with R code
        #' @param ... Other arguments, currently ignored
        eval = function(code,...){
            
            perc_match <- getMatch(code,regexec("^%%([a-zA-Z0-9]+)\n",code))
            if(length(perc_match) > 1){
                magic <- perc_match[2]
                # message(sprintf("Found magic '%s'",magic))
                code <- gsub("^%%.+?\n","",code)
                private$handle_magic(magic,code)
                return()
            }

            if("var_dic_list" %in% objects(envir=.GlobalEnv)) 
                rm(var_dic_list,envir=.GlobalEnv)
            #pos <- match("RKernel",search())
            #assign("var_dic_list",private$var_dic_list,pos=pos)

            if(private$nframes < 0){
                getnframes <- function(e) private$nframes <- sys.nframe()
                tryCatch(evalq(stop()),
                    error = getnframes)
            }

            private$results <- list()
            if(private$aborted) return(private$results)

            expressions <- try(parse(text=code),silent=TRUE)
            if(inherits(expressions,"try-error")){
                condition <- attr(expressions,"condition")
                private$status <- "error"
                private$kernel$stream(text=condition$message,
                                      stream="stderr")
            }
            else {
                private$new_cell <- TRUE
                private$context$evaluate(expressions,envir=.GlobalEnv)

                if(length(private$saved.options)){
                    op <- private$saved.options
                    private$saved.options <- list()
                    do.call("options",op)
                }
                if(length(private$saved.parms)){
                    op <- private$saved.parms
                    private$saved.parms <- list()
                    do.call("par",op)
                }

                private$run_callbacks()
            }
        },
        #' @description
        #' Get the payload associated with the result returned from running a Jupyter cell
        #' @param clear A logical value, whether the payload list should be cleared after returning it.
        get_payload = function(clear=FALSE){
            payload <- private$payload
            if(clear)
                private$payload <- list()
            return(payload)
        },
        #' @description
        #' Get the execution status ("ok", "error", or "aborted")
        #' @param reset A logical value, whether the status should be reset to "ok" after returning it.
        get_status = function(reset=FALSE){
            status <- private$status
            if(reset)
                private$status <- "ok"
            return(status)
        },
        #' @description
        #' Set the execution status
        #' @param status A character string, the status to be set ("ok", "error", or "aborted")
        set_status = function(status){
            private$status <- status
        },
        #' @description
        #' Check whether the evaluation of expression is aborted - if TRUE, the execution of the notebook is aborted
        #' instead of running to the end (if the user has requested to run all cells of the notebook).
        #' @param reset A logical value, whether the status should be reset to FALSE after returning it.
        is_aborted = function(reset=FALSE){
            aborted <- private$aborted
            if(reset)
                private$aborted <- FALSE
            return(aborted)
        },
        #' @description
        #' Quit the R session in the kernel. Roughly equivalent to \code{\link[base]{quit}}, but
        #' tells the kernel manager of the jupyter server to shut down the kernel.
        #' @param ... Any kind of argument, ignored.
        quit = function(...){
            payload <- list(source="ask_exit",
                            keepkernel=FALSE)
            private$add_payload(payload)
        },
        #' @description
        #' Stream text to the frontend.
        #' @param text Text to be sent to the frontend
        #' @param stream A string to select the stream -- either "stout" or "stderr"
        stream = function(text, stream=c("stdout","stderr")){
            stream <- match.arg(stream)
            private$kernel$stream(text=text,
                                  stream=stream)
        },
        #' @description
        #' Clear the current output cell in the frontend.
        #' @param wait Logical value, whether to wait until output is cleared.
        clear_output = function(wait=FALSE){
            private$kernel$clear_output(wait=wait)
        },
        #' @description
        #' Check whether code is syntactically complete.
        #' @param code A character string with code to be checked.
        code_is_complete = function(code){
            status <- tryCatch({
                parse(text=code)
                "complete"
            },
            error = conditionMessage)
            if(is_unexpected_end(status) || is_unexpected_string(status))
                return("incomplete")
            else if(status!="complete")
                return("invalid")
            else return("complete")
        },
        #' @description
        #' Provide completion for code given at point.
        #' @param code A character string with code to be checked for
        #'    completions.
        #' @param cursor_pos An integer, the current position of the cursor.
        get_completions = function(code,cursor_pos){
            if(!private$completions_inited) private$init_completions()

# FIXME Rare error message
#Warning in max(which(cursor_pos <= line_end)) :
#  no non-missing arguments to max; returning -Inf
#Error in checkHT(n, dx <- dim(x)) : 
#  invalid 'n' -  must contain at least one non-missing element, got none.
#Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> head.default -> checkHT


            lines <- splitLines(code)
            llines <- nchar(lines)
            line_end <- cumsum(llines)
            line_start <- head(c(0,line_end),-1)
            i <- max(which(cursor_pos <= line_end))
            pos <- cursor_pos - line_start[i] + 1
            line <- lines[i]
            private$cf$assignLinebuffer(line)
            private$cf$assignEnd(pos)
            match_info <- private$cf$guessTokenFromLine(update=FALSE)
            private$cf$guessTokenFromLine()
            private$cf$completeToken()

            matches <- private$cf$retrieveCompletions()
            start <- line_start[i] + match_info$start
            end <- start + nchar(match_info$token)

            return(list(
                matches = matches,
                start = start,
                end = end
            ))
        },
        #' @description
        #' Set a handler to be called when an expression has been evaluated.
        #' @param handler A function.
        #' @param remove A logical value, whether the handler should be
        #'    removed.
        on_eval = function(handler,remove=FALSE){
            private$callbacks$register(handler,remove=remove)
        },
        #' @description
        #' Set options locally for the current jupyter notebook cell
        #' @param ... Options, see \code{\link{options}}.
        cell.options = function(...){
            op <- options()
            private$saved.options <- op
            args <- list(...)
            nms <- names(args)
            op[nms] <- args
            do.call("options",op)
        },
        #' @description
        #' Set graphics parameters locally for the current jupyter notebook cell
        #' @param ... Graphics parameters, see \code{\link{options}}.
        cell.par = function(...){
            op <- par(no.readonly=TRUE)
            private$saved.parms <- op
            args <- list(...)
            nms <- names(args)
            op[nms] <- args
            do.call("par",op)
        }

    ),
    
    private = list(
        kernel=list(),
        comm_dispatcher=list(),
        output=list(),

        nframes = -1,
        aborted = FALSE,
        status = "ok",
        payload = list(),
        results = list(),
        env = list(),
        comm_manager = list(),

        context = list(),


        help_port = integer(),
        help_proc = integer(),
        help_url = character(),

        start_local_help_system = function(help_port=getOption("help.port",10001)){
            help_port <- as.integer(help_port)
            if(!check_help_port(help_port)){
                help_proc  <- callr::r_bg(function(port){
                    options(help.ports=port)
                    tools::startDynamicHelp(TRUE)
                    repeat Sys.sleep(60)
                },args=list(port=help_port))
                repeat{
                    if(help_proc$is_alive()) break
                }
                private$help_proc <- help_proc
            }
            #private$help_port <- help_port
            private$help_url <- paste0("http://127.0.0.1:",help_port)
            #assign("help_proc",private$help_proc,envir=private$env)
            #assign("help_port",private$help_port,envir=private$env)

            assign("get_help_url",function()private$help_url,envir=private$env)
        },
        start_proxied_help_system = function(help_url=paste0(Sys.getenv("JUPYTERHUB_SERVICE_PREFIX"),
                                                           "/RHelp")){
            help_url <- gsub("//","/",help_url,fixed=TRUE)
            private$help_url <- help_url
            assign("get_help_url",function()private$help_url,envir=private$env)
        },
        start_help_system = function(){
            if(nzchar(Sys.getenv("JUPYTERHUB_SERVICE_PREFIX")) &&
               !length(getOption("rkernel_help_system")))
                options(rkernel_help_system="proxy")
            if(isTRUE(getOption("rkernel_help_system")=="proxy"))
                private$start_proxied_help_system()
            else
                private$start_local_help_system()
        },
        
        # help_start = function(update = FALSE, 
        #                       gui = "irrelevant", 
        #                       browser = getOption("browser"), 
        #                       remote = NULL){
        #     if(is.null(remote))
        #         remote <- private$help_url
        #     utils::help.start(update=update,
        #                       gui=gui,
        #                       browser=browser,
        #                       remote=remote)
        # },

        new_cell = TRUE,

        add_payload = function(payload){
            private$payload <- append(private$payload,list(payload))
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
            p <- Page("text/plain"=text)
            private$add_payload(unclass(p))
        },

        handle_text = function(text) {
            # log_out(sprintf("handle_text(%s)",text))
            # log_out("handle_text")
            # log_out(text,use.print=TRUE)
            text <- paste(text,collapse="\n")
            if(nzchar(text))
                private$kernel$stream(text = text,
                                      stream = "stdout")
        },

        last_plot_id = character(),

        handle_graphics = function(plt,update=FALSE) {

            # log_out(sprintf("evaluator$handle_graphics(...,update=%s)",if(update)"TRUE"else"FALSE"))
            new_display <- !update || private$new_cell && !getOption("jupyter.update.graphics",TRUE)
            
            width      <- getOption("jupyter.plot.width",6)
            height     <- getOption("jupyter.plot.height",6)
            pointsize  <- getOption("jupyter.plot.pointsize",12)
            resolution <- getOption("jupyter.plot.res",150)
            scale      <- getOption("jupyter.plot.scale",0.5)
            units      <- getOption("jupyter.plot.units","units")


            rkernel_graphics_types <- getOption("jupyter.graphics.types")

            mime_data <- list()
            mime_metadata <- list()

            for(mime in rkernel_graphics_types){
                repr_func <- mime2repr[[mime]]
                mime_data[[mime]] <- repr_func(plt,
                                          width=width,
                                          height=height,
                                          pointsize=pointsize,
                                          res=resolution)
                mime_metadata[[mime]] <- list(
                    width=width*resolution*scale,
                    height=height*resolution*scale)
                if(mime=="image/svg+xml")
                    mime_metadata[[mime]]$isolated <-TRUE
            }

            if(new_display) {
                id <- UUIDgenerate()
                private$last_plot_id <- id
                cls <- "display_data"
            }
            else {
                id <- private$last_plot_id
                cls <- "update_display_data"
            } 

            d <- list(data = mime_data,
                      metadata = mime_metadata,
                      transient = list(display_id=id))
            class(d) <- cls

            # log_out(str(d),use.print=TRUE)
            private$kernel$display_send(d)

            private$new_cell <- FALSE

        },

        handle_message = function(m) {
            text <- conditionMessage(m)
            text <- paste(text,collapse="\n")
            private$kernel$stream(text = text,
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
            private$kernel$stream(text = text,
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
            private$status <- "error"
            private$kernel$stream(text = text,
                                  stream = "stderr")
            stop_on_error <- getOption("rkernel_stop_on_error")
            if(stop_on_error){
                calls <- sys.calls()
                private$aborted <- TRUE
                #drop_prev <- private$nframes - 2
                #calls <- tail(calls,-drop_prev)
                calls <- tail(calls,-15)
                calls <- head(calls,-3)
                calls <- limitedLabels(calls)
                if(length(calls))
                    calls <- paste0(format(seq_along(calls),justify="right"),
                                    ". ",
                                    calls)
                traceback <- c("\nTraceback:",calls)
                traceback <- paste(traceback,collapse="\n")
                # private$kernel$stream(text = traceback,
                #                       stream = "stderr")
                private$kernel$send_error(name = "ERROR",
                                          value = "",
                                          traceback = list(traceback)
                                          )
            }
        },

        handle_value = function(x,visible) {
            if(visible){
                if(any(class(x) %in% getOption("rkernel_paged_classes"))){
                    displayed <- display_data(x)
                    payload <- list(source="page",
                                    data=displayed$data,
                                    start=1)
                    private$add_payload(payload)
                }
                else if(any(class(x) %in% getOption("rkernel_displayed_classes"))){
                    d <- display_data(x)
                    private$kernel$display_data(data=d$data,
                                                metadata=d$metadata,
                                                transient=d$transient)
                }
                else if(inherits(x,"clear_output")){
                    private$kernel$clear_output(x)
                }
                # else if(inherits(x,"execute_result")){
                #     private$kernel$execute_result(data=x$data,
                #                                   metadata=x$metadata)
                # }
                else if(inherits(x,"display_data")){
                    private$kernel$display_data(data=x$data,
                                                metadata=x$metadata,
                                                transient=x$transient)
                }
                else if(inherits(x,"update_display_data")){
                    private$kernel$update_display_data(data=x$data,
                                                       metadata=x$metadata,
                                                       transient=x$transient)
                }
                else if(inherits(x,"payload")){
                    private$add_payload(unclass(x))
                }
                else {
                    text <- capture.output(print(x))
                    text <- paste(text,collapse="\n")
                    #private$kernel$stream(text = text,
                    #               stream = "stdout")
                    data <- list("text/plain"=text)
                    private$kernel$execute_result(data=data)
                }
            }
        },

        handle_magic = function(magic,code){
            if(tolower(magic) == "math"){
                d <- LaTeXMath(code)
                private$kernel$display_data(data=d$data,
                                            metadata=d$metadata,
                                            transient=d$transient)
            } 
            else if (tolower(magic) == "css"){
                d <- CSS(code)
                private$kernel$display_data(data=d$data,
                                            metadata=d$metadata,
                                            transient=d$transient)
            }
            else if (tolower(magic) == "javascript"){
                d <- Javascript(code)
                private$kernel$display_data(data=d$data,
                                            metadata=d$metadata,
                                            transient=d$transient)
            }
            else if (tolower(magic) == "html"){
                d <- raw_html(code)
                private$kernel$display_data(data=d$data,
                                            metadata=d$metadata,
                                            transient=d$transient)
            }
        },


        handle_interrupt = function(i){
            private$kernel$stream(text = "<interrupted>",
                                  stream = "stderr")
            private$status <- "aborted"
            private$aborted <- TRUE
        },


        completions_inited = FALSE,
        cf = list(),

        init_completions = function(){
            utils_ns <- asNamespace('utils')
            private$cf$assignLinebuffer <- get(".assignLinebuffer",utils_ns)
            private$cf$assignEnd <- get(".assignEnd",utils_ns)
            private$cf$guessTokenFromLine <- get(".guessTokenFromLine",utils_ns)
            private$cf$completeToken <- get(".completeToken",utils_ns)
            private$cf$retrieveCompletions <- get(".retrieveCompletions",utils_ns)
            private$completions_inited <- TRUE
        },

        var_dic_list = function(){ 
            ll <- ls(.GlobalEnv, all.names = FALSE)
            varList <- list()
            n <- 1
            for (k in ll){
                class <- class(get(k)) 
                rk <- paste(capture.output(str(get(k))),collapse="\n")
                size <-  object.size(get(k))
                sk <- substr(rk,0, 200); 
                l <- list(varName = k, 
                          varType = class, 
                          varSize = size, 
                          varContent = sk)
                varList[[n]] <- l
                n = n + 1
                }
            return(toJSON(varList, 
                          simplifyVector=FALSE, 
                          force=TRUE))
        },

        saved.options = list(),
        saved.parms = list(),

        cat = function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
                        append = FALSE){
            if(!missing(file))
                base::cat(...,file,sep,fill,labels,append)
            else {
                text <- paste(...,sep=sep)
                if(fill){
                    width <- if(is.numeric(fill)) fill else getOption("width")
                    text <- strwrap(text,width=width)
                    text <- paste(text,collapse="\n")
                }
                private$kernel$stream(text=text,
                                      stream="stdout")
            }
        },

        callbacks = NULL,
        run_callbacks = function(){
            if(is.null(private$callbacks))
                private$callbacks <- CallbackDispatcher()
            else 
                private$callbacks$run()
        }

    )

)

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

is_unexpected_end <- function(code) 
    grepl(gettext("unexpected end of input",
                  domain = "R"),
          code,fixed = TRUE)

is_unexpected_string <- function(code) 
    grepl(gettextf("unexpected %s","INCOMPLETE_STRING",
                  domain = "R"),
          code,fixed = TRUE)


splitLines <- function(text) strsplit(text,"\n",fixed=TRUE)[[1]]

get_evaluator <- function() {
    kernel <- get_current_kernel()
    kernel$evaluator
}

#' Set options or graphics parameters locally for the current jupyter notebook cell
#' @param ... Options, see \code{\link{options}}.
#' @export
cell.options <- function(...){
    e <- get_evaluator()
    e$cell.options(...)
}

#' @rdname cell.options 
#' @param ... Graphics parameters, see \code{\link{options}}.
#' @export
cell.par <- function(...){
    e <- get_evaluator()
    e$cell.par(...)
}
