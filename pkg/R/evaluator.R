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

#' @export
Evaluator <- R6Class("Evaluator",
    public = list(
 
        nframes = -1,
        aborted = FALSE,
        status = "ok",
        payload = list(),
        results = list(),
        env = list(),
        comm_manager = list(),

        context = list(),

        initialize = function(kernel){
            private$kernel <- kernel
        },

        startup = function(...) {

            self$env <- new.env()
            #attach(self$env,name="RKernel")
            #pos <- match("RKernel",search())
            assign("q",self$quit,envir=self$env)
            assign("quit",self$quit,envir=self$env)
            assign("interactive",function()TRUE,envir=self$env)
            # assign("cell.options",self$cell.options,pos=pos)
            # assign("cell.par",self$cell.par,pos=pos)
            # 
            assign("display",self$display,envir=self$env)
            # assign("display",display,envir=self$env)
            # assign("stream",self$stream,pos=pos)
            # #assign("cat",self$cat,pos=pos)
            # #assign("print",self$print,pos=pos)
            # 
            # assign("Javascript",Javascript,pos=pos)
            # assign("Math",LaTeXMath,pos=pos)
            # assign("raw_html",raw_html,pos=pos)
            # assign("Page",Page,pos=pos)
            assign("View",View,envir=self$env)
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
                    rkernel_stop_on_error=TRUE)


            add_paged_classes(c("help_files_with_topic","packageIQR","hsearch"))
            add_displayed_classes(c("htmlwidget","html_elem","shiny.tag"))

            private$comm_dispatcher <- private$kernel$comm_dispatcher

            self$context <- Context$new(text_callback=self$handle_text,
                                        message_callback=self$handle_message,
                                        warning_callback=self$handle_warning,
                                        error_callback=self$handle_error,
                                        value_callback=self$handle_value,
                                        graphics_callback=self$handle_graphics,
                                        envir=.GlobalEnv,
                                        attachment=self$env)
            
            self$start_help_system()
            assign("help_proc",self$help_proc,envir=self$env)
            assign("help_port",self$help_port,envir=self$env)
            assign("help.start",self$help_start,envir=self$env)

            assign("get_help_url",function()self$help_url,envir=self$env)

            suppressMessages(trace(example,tracer=quote(if(missing(run.donttest)) run.donttest<-TRUE),print=FALSE))
        },

        help_port = integer(),
        help_proc = integer(),
        help_url = character(),

        start_help_system = function(help_port=getOption("help.port",10001)){
            # if(is.null(help_port)){
            #     repeat{
            #         help_port <- tools::startDynamicHelp(TRUE)
            #         if(help_port > 0) break
            #     }
            #     port0 <- tools::startDynamicHelp(FALSE)
            #     stopifnot(port0 == 0)
            # }
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
                self$help_proc <- help_proc
            }
            self$help_port <- help_port
            self$help_url <- paste0("http://127.0.0.1:",help_port)
        },

        help_start = function(update = FALSE, 
                              gui = "irrelevant", 
                              browser = getOption("browser"), 
                              remote = NULL){
            if(is.null(remote))
                remote <- self$help_url
            utils::help.start(update=update,
                              gui=gui,
                              browser=browser,
                              remote=remote)
        },

        shutdown = function(){
            base::q()
        },

        new_cell = TRUE,
        eval = function(code,...,silent=FALSE){
            
            perc_match <- getMatch(code,regexec("^%%(.+?)\n\n",code))
            if(length(perc_match) > 1){
                magic <- perc_match[2]
                # message(sprintf("Found magic '%s'",magic))
                code <- gsub("^%%.+?\n","",code)
                self$handle_magic(magic,code)
                return()
            }

            if("var_dic_list" %in% objects(envir=.GlobalEnv)) 
                rm(var_dic_list,envir=.GlobalEnv)
            #pos <- match("RKernel",search())
            #assign("var_dic_list",self$var_dic_list,pos=pos)

            if(self$nframes < 0){
                getnframes <- function(e) self$nframes <- sys.nframe()
                tryCatch(evalq(stop()),
                    error = getnframes)
            }

            self$results <- list()
            if(self$aborted) return(self$results)

            expressions <- try(parse(text=code),silent=TRUE)
            if(inherits(expressions,"try-error")){
                condition <- attr(expressions,"condition")
                self$status <- "error"
                private$kernel$stream(text=condition$message,
                                      stream="stderr")
            }
            else {
                self$new_cell <- TRUE
                self$context$evaluate(expressions,envir=.GlobalEnv)

                if(length(self$saved.options)){
                    op <- self$saved.options
                    self$saved.options <- list()
                    do.call("options",op)
                }
                if(length(self$saved.parms)){
                    op <- self$saved.parms
                    self$saved.parms <- list()
                    do.call("par",op)
                }

                self$run_callbacks()
            }
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

        set_status = function(status){
            self$status <- status
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
            p <- Page("text/plain"=text)
            self$add_payload(unclass(p))
        },

        handle_text = function(text) {
            # log_out(sprintf("handle_text(%s)",text))
            # log_out("handle_text")
            # log_out(text,use.print=TRUE)
            text <- paste(text,collapse="\n")
            private$kernel$stream(text = text,
                                  stream = "stdout")
        },

        last_plot_id = character(),

        handle_graphics = function(plt,update=FALSE) {

            # log_out(sprintf("evaluator$handle_graphics(...,update=%s)",if(update)"TRUE"else"FALSE"))
            new_display <- !update || self$new_cell && !getOption("jupyter.update.graphics",TRUE)
            
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
                self$last_plot_id <- id
                cls <- "display_data"
            }
            else {
                id <- self$last_plot_id
                cls <- "update_display_data"
            } 

            d <- list(data = mime_data,
                      metadata = mime_metadata,
                      transient = list(display_id=id))
            class(d) <- cls

            # log_out(str(d),use.print=TRUE)
            private$kernel$display_send(d)

            self$new_cell <- FALSE

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
            self$status <- "error"
            private$kernel$stream(text = text,
                                  stream = "stderr")
            stop_on_error <- getOption("rkernel_stop_on_error")
            if(stop_on_error){
                calls <- sys.calls()
                self$aborted <- TRUE
                #drop_prev <- self$nframes - 2
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
                    self$add_payload(payload)
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
                    self$add_payload(unclass(x))
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
            self$status <- "aborted"
            self$aborted <- TRUE
        },

        display = function(...){
            d <- display_data(...)
            private$kernel$display_data(data=d$data,
                                        metadata=d$metadata,
                                        transient=d$transient)
        },

        stream = function(text, stream=c("stdout","stderr")){
            stream <- match.arg(stream)
            private$kernel$stream(text=text,
                                  stream=stream)
        },

        clear_output = function(wait=FALSE){
            private$kernel$clear_output(wait=wait)
        },

        set_last_value = function(x){
            pos <- match("RKernel",search())
            assign(".Last.value",x,pos=pos)
        },

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

        completions_inited = FALSE,
        cf = list(),

        init_completions = function(){
            utils_ns <- asNamespace('utils')
            self$cf$assignLinebuffer <- get(".assignLinebuffer",utils_ns)
            self$cf$assignEnd <- get(".assignEnd",utils_ns)
            self$cf$guessTokenFromLine <- get(".guessTokenFromLine",utils_ns)
            self$cf$completeToken <- get(".completeToken",utils_ns)
            self$cf$retrieveCompletions <- get(".retrieveCompletions",utils_ns)
            self$completions_inited <- TRUE
        },
        
        get_completions = function(code,cursor_pos){
            if(!self$completions_inited) self$init_completions()

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
            self$cf$assignLinebuffer(line)
            self$cf$assignEnd(pos)
            match_info <- self$cf$guessTokenFromLine(update=FALSE)
            self$cf$guessTokenFromLine()
            self$cf$completeToken()

            matches <- self$cf$retrieveCompletions()
            start <- line_start[i] + match_info$start
            end <- start + nchar(match_info$token)

            return(list(
                matches = matches,
                start = start,
                end = end
            ))
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
        cell.options = function(...){
            op <- options()
            self$saved.options <- op
            args <- list(...)
            nms <- names(args)
            op[nms] <- args
            do.call("options",op)
        },

        saved.parms = list(),
        cell.par = function(...){
            op <- par(no.readonly=TRUE)
            self$saved.parms <- op
            args <- list(...)
            nms <- names(args)
            op[nms] <- args
            do.call("par",op)
        },

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

        init_env_browser = function(){
            d <- init_env_browser()
            private$kernel$display_data(d$data)
        },

        callbacks = NULL,
        run_callbacks = function(){
            if(is.null(self$callbacks))
                self$callbacks <- CallbackDispatcher()
            else 
                self$callbacks$run()
        },
        on_eval = function(handler,remove=FALSE){
            self$callbacks$register(handler,remove=remove)
        }
    ),
    
    private = list(
        kernel=list(),
        comm_dispatcher=list(),
        output=list()
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
