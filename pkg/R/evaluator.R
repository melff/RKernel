#' @include dictionary.R

#' @importFrom utils capture.output
#' @importFrom grDevices png

check_help_port <- function(port){
    url <- sprintf("http://127.0.0.1:%d/doc/html/index.html",port)
    # log_out(sprintf("Trying %s",url))
    res <- try(curlGetHeaders(url),silent=TRUE)
    if(inherits(res,"try-error")) return(FALSE)
    if(attr(res,"status") == 200) return(TRUE)
    return(FALSE)
}


evaluator <- new.env()

exports <- new.env()
register_export <- function(obj,name=deparse(substitute(obj))){
    if(length(name) >1 )
        for(n in name)
            assign(n,obj,envir=exports)
    else
        assign(name,obj,envir=exports)
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
            evaluator$current <- self
        },
        #' @description
        #' Prepare the object for evaluating expressions
        #' @param ... Any kind of arguments, ignored.
        startup = function(...) {

            private$env <- new.env()
            #attach(private$env,name="RKernel")
            #pos <- match("RKernel",search())
            register_export(function()TRUE,"interactive")
            register_export(self$quit,c("q","quit"))
            # 
            # assign("display",self$display,envir=private$env)
            # assign("display",display,envir=private$env)
            register_export(self$stream,"stream")
            # #assign("cat",private$cat,pos=pos)
            register_export(private$kernel$poll_and_respond,"poll_and_respond")
            # assign("Javascript",Javascript,pos=pos)
            # assign("Math",LaTeXMath,pos=pos)
            # assign("raw_html",raw_html,pos=pos)
            # assign("Page",Page,pos=pos)
            ## assign("print",private$print,envir=private$env)
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
            register_export(self$cells,"In")
            register_export(self$cells_results,"Out")

            options(crayon.enabled=TRUE,crayon.colors=256L,
                    jupyter.graphics.types=c("image/png","application/pdf"),
                    jupyter.plot.width=6,
                    jupyter.plot.height=6,
                    jupyter.plot.pointsize=12,
                    jupyter.plot.res=150,
                    jupyter.plot.units="in",
                    jupyter.plot.scale=0.5,
                    jupyter.update.graphics=TRUE,
                    pager=private$pager,
                    repos=c(CRAN="https://cloud.r-project.org"),
                    rkernel_stop_on_error=TRUE)


            add_displayed_classes(c("help_files_with_topic","packageIQR","hsearch"))
            add_displayed_classes(c("htmlwidget","html_elem","shiny.tag","iframe","dataTable"))

            private$comm_dispatcher <- private$kernel$comm_dispatcher
            
            private$context <- Context$new(envir=.GlobalEnv,
                                           attachment=private$env)
            
            private$context$on_eval(private$handle_eval)
            private$context$on_result(private$handle_result)
            private$context$on_message(private$handle_message)
            private$context$on_warning(private$handle_warning)
            private$context$on_error(private$handle_error)

            private$context$on_print(private$handle_graphics,exit=private$handle_text)
            private$context$on_cat(private$handle_graphics,exit=private$handle_text)

            private$context$on_enter(private$handle_context_enter)
            private$context$on_exit(private$handle_context_exit)
            private$graphics <- GraphicsDevice$new()

            home_dir <- Sys.getenv("HOME")
            jupyter_config <- file.path(home_dir,".jupyter","RKernel-config.R")
            if(file.exists(jupyter_config)) 
                source(jupyter_config)

            parents <- strsplit(getwd(),.Platform$file.sep)[[1]]
            parents <- Reduce(file.path,parents,accumulate=TRUE)
            for(parent in parents){
                dot_file <- file.path(parent,".RKernel-profile")
                if(file.exists(dot_file))
                    source(dot_file)
                startup_file <- file.path(parent,"RKernel-startup.R")
                if(file.exists(startup_file))
                    source(startup_file)
            }
            # Sleep briefly to allow HTTP service etc.
            private$kernel$add_service(function()Sys.sleep(getOption("rkernel_service_interval",.001)))
            self$start_httpd()
            private$start_help_system()
            register_export(private$get_help_url,"get_help_url")
            register_export(private$get_url,"get_url")
            register_export(private$get_port,"get_port")
            register_export(private$get_shared_help_port,"get_shared_help_port")
            register_export(private$set_port,"set_port")
            register_export(self$str2iframe,"str2iframe")
            register_export(self$readline,"readline")
            register_export(self$str,"str")
            register_export(self$restart_graphics,"restart_graphics")
            private$assign_exports()
            em <- EventManager(type="eval")
            em$activate()
            # log_out("evaluator$startup() complete")
            private$jupyterhub_prefix <- Sys.getenv("JUPYTERHUB_SERVICE_PREFIX")

        },
        #' @description
        #' Shut the session down
        shutdown = function(){
            base::q(save="no")
        },
        #' @description
        #' Evaluate R code
        #' @param code A string with R code
        #' @param ... Other arguments, currently ignored
        eval_cell = function(code,allow_stdin,...){
            
            private$allow_stdin <- allow_stdin
            # log_out("++ eval_cell ++++++++++++++++++++++++++++++++++++")
            mparsed <- parse_magic(code)
            if(length(mparsed) > 0){
                magic <- mparsed$magic
                args <- mparsed$args
                code <- mparsed$code
                private$handle_magic(magic,code,args)
                eventmanagers$eval$send("cell_completed")
                return()
            }

            # if("var_dic_list" %in% objects(envir=.GlobalEnv)) 
            #     rm(var_dic_list,envir=.GlobalEnv)
            #pos <- match("RKernel",search())
            #assign("var_dic_list",private$var_dic_list,pos=pos)

            if(private$nframes < 0){
                getnframes <- function(e) private$nframes <- sys.nframe()
                tryCatch(evalq(stop()),
                    error = getnframes)
            }
            # log_out(private$nframes)
            # log_out(sys.calls(),use.print=TRUE)
            # log_out(sys.frames(),use.print=TRUE)
            # log_out(sys.parents(),use.print=TRUE)

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
                self$cell_no <- self$cell_no + 1
                # log_out(sprintf("== BEGIN CELL [%d] ==",self$cell_no))
                private$context$evaluate(expressions,envir=.GlobalEnv)
                current_value <- private$context$last.value
                self$cells[[self$cell_no]] <- code
                self$cell_results[[self$cell_no]] <- current_value$value
                assign("In",self$cells,envir=private$env)
                assign("Out",self$cell_results,envir=private$env)
                assign(".Last.value",current_value$value,envir=private$env)

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
                eventmanagers$eval$send("cell_completed")
                # log_out(sprintf("== END CELL [%d] ==",self$cell_no))
            }
        },
        #' @field cell_no The number of the cell currently executed
        cell_no = 0,
        #' @field cells A 'dictionary' traitlet with the code of the 
        #'   cells so far encountered
        cells = dictionary(),
        #' @field cells A 'dictionary' traitlet with the output of the 
        #'   code cells so far executed
        cell_results = dictionary(),
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

            lines <- splitLines(code)
            line_length <- nchar(lines)
            line_start <- head(c(0,cumsum(line_length + 1)),-1)
            line_end <- line_start + line_length
            suppressWarnings(i <- which(line_start <= cursor_pos & cursor_pos <= line_end))
            if(!length(i) || !is.finite(i)) return(NULL)
            pos <- cursor_pos - line_start[i] 
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
                matches = as.list(matches),
                start = start,
                end = end
            ))
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
        },
        #' @description
        #' A variant of the 'print' function that allows to run hooks before and after
        #' after printing. Sends the events "before_print" and "print", see \code{\link{EventManager}}
        #' @param x The object to be printed
        #' @param ... Any additional arguments, see \code{\link{print}}
        print = function(x,...){
            eventmanagers$output$send("before_print",x,...)
            private$kernel$print(x,...)
            eventmanagers$output$send("print",x,...)
        },
        #' @description
        #' A variant of the 'cat' function that allows to run hooks before and after
        #' after creating output. Sends the events "before_print" and "print", see \code{\link{EventManager}}
        #' @param ... Any strings to be output, see \code{\link{cat}}
        #' @param file A file name, see \code{\link{cat}}
        #' @param sep A separtor string, see \code{\link{cat}}
        #' @param fill A logical value, see \code{\link{cat}}
        #' @param labels See \code{\link{cat}}
        #' @param append A logical value, see \code{\link{cat}}
        cat = function(..., file = "", sep = " ", fill = FALSE, labels = NULL, 
                       append = FALSE){
            do_send <- !nzchar(file)
            if(do_send) eventmanagers$output$send("before_cat",...,
                                                  sep=sep,
                                                  fill=fill,
                                                  labels=labels,
                                                  append=append)
            private$kernel$cat(...,file=file,sep=sep,
                               fill=fill,labels=labels,
                               append=append)
            if(do_send) eventmanagers$output$send("cat",...,
                                                  sep=sep,
                                                  fill=fill,
                                                  labels=lables,
                                                  append=append)
        },
        #' @description
        #' A variant of the 'str' function that allows to run hooks before and after
        #' after creating output.
        #' @param object Any object
        #' @param ... Any additional arguments, see \code{\link{str}}
        str = function(object, ...){
            private$handle_str()
            private$kernel$str(object,...)
            private$handle_str_exit()
        },
        #' @description
        #' A variant of 'tools:::httpd' that adapts the paths used in HTML-pages if 
        #' the help system is proxied. Also allows to implement handlers
        #' for specific URL patterns via the event mechanism (see \code{\link{EventManager}})
        #' @param path The \emph{relative} url, e.g. "/doc/html/something/index.html"
        #' @param query The query string (that appeared after '?' in the http request)
        #' @param ... Any further arguments, passed on to specific handlers
        httpd = function(path,query,...){
            # log_out("http path: ",path)
            # log_out("http query: ",query,use.str=TRUE)
            # log_out(eventmanagers$http,use.str=TRUE)
            split_path <- strsplit(path,"/")[[1]]
            response <- NULL
            if(length(split_path) > 1){
                event <- split_path[2]
                response <- eventmanagers$http$send(event,path,query,...)
            }
            if(!length(response)){
                response <- private$kernel$httpd(path=path,query=query,...)
                payload <- response$payload
                payload <- gsub("/doc/html/",paste0(private$http_url,
                                                    "/doc/html/"),payload,fixed=TRUE)
                response$payload <- payload
            }
            return(response)
        },
        #' @description
        #' Get the URL served by the current R kernel
        get_url = function(){
            if(private$http_port == 0){
                self$start_httpd()
            }
            return(private$http_url)
        },
        #' @description
        #' Get the ip port served by the current R kernel
        get_port = function(){
            if(private$http_port == 0){
                self$start_httpd()
            }
            return(private$http_port)
        },
        #' @description
        #' Set the ip port served by the current R kernel
        #' @param port The port number, an integer
        set_port = function(port){
            if(private$http_port > 0)
                suppressMessages(tools::startDynamicHelp(FALSE))
            self$start_httpd(port=port)
        },
        #' @description
        #' Start the help server of the R kernel
        #' @param port The port number, an integer; or NULL
        start_httpd = function(port=NULL){
            if(length(port) > 0)
                options(help.ports=port[1])
            if(private$http_port == 0){
                suppressMessages(port <- tools::startDynamicHelp(TRUE))
                private$http_port <- port
                http_url <- sprintf("/proxy/%d",port)
                if(nzchar(Sys.getenv("JUPYTERHUB_SERVICE_PREFIX"))){
                    http_url <- paste0(Sys.getenv("JUPYTERHUB_SERVICE_PREFIX"),
                                       http_url)
                    http_url <- gsub("//","/",http_url,fixed=TRUE)
                }
                private$http_url <- http_url
                em <- EventManager(type="http")
                em$activate()
            }
        },
        #' @description
        #' Create an HTML iframe tag that refers to some (usually HTML) code
        #' @param code The code to be shown in the iframe
        #' @param width The intended width of the iframe, a string or a number
        #' @param height The intended height of the iframe, a string or a number
        #' @param class The DOM class attribute the iframe, a string
        #' @param style The CSS style attribte of the iframe, a string
        str2iframe = function(code,
                              width = "100%",
                              height = 400L,
                              class = "rkernel-iframe",
                              style = "border-style:none",...){
            cl <- match.call()
            log_out(deparse1(cl))
            id <- UUIDgenerate()
            path <- paste0("/iframe/",id,"/")
            url <- paste0(self$get_url(),path)
            if(!eventmanagers$http$has("iframe"))
                eventmanagers$http$on("iframe",private$handle_iframe)
            private$iframes[[id]] <- code
            if_tmpl <- "
                   <iframe src=\"%s\" width=\"%s\" height=\"%s\" class=\"%s\" style=\"%s\">
                   </iframe>
                   "
            iframe <- sprintf(if_tmpl,url,width,height,class,style)
            # mime_data <- list(
            #     "text/plain" = "",
            #     "text/html"  = iframe
            # )
            # metadata <- emptyNamedList
            # transient <- list(display_id=id)
            # display_data(data=mime_data,
            #              metadata=metadata,
            #              transient=transient)
            structure(iframe,class="iframe",id=id)
        },
        #' @description
        #' Ask the kernel object to send an input request to the frontend
        #' @param prompt A character string
        readline = function(prompt=""){
            if(!private$allow_stdin) return()
            private$kernel$input_request(prompt=prompt,
                                         password=FALSE)
            result <- private$kernel$read_stdin()
            return(result)
        },
        #' Restart/recreate the graphics device if for whatever reason 
        #' it was closed needs to be reinitialized
        restart_graphics = function(){
            dev_cur <- dev.cur()
            if(dev_cur > 1)
                dev.off(dev_cur)
            private$graphics$create()
            private$graphics$activate()
        }
    ),
    
    private = list(
        kernel=list(),
        comm_dispatcher=list(),
        graphics = list(),
        last_plot = NULL,
        current_plot = NULL,

        nframes = -1,
        aborted = FALSE,
        status = "ok",
        payload = list(),
        results = list(),
        env = list(),
        comm_manager = list(),

        allow_stdin = TRUE,

        context = list(),

        help_port = integer(),
        help_proc = integer(),
        help_url = character(),

        http_port = 0,
        http_url = character(),

        start_shared_help_system = function(help_port=getOption("help.port",NA)){
            if(is.finite(help_port))
                help_port <- as.integer(help_port)
            else
                help_port <- private$get_shared_help_port()
            if(!check_help_port(help_port)){
                # log_out("help port not serviced, attempting to start new server")
                private$start_shared_help_server(help_port)
                # check_help_port(help_port)
                # while(!check_help_port(help_port))
                #     Sys.sleep(.2)
                help_port <- private$get_shared_help_port()
            } else {
                private$publish_shared_help_port(help_port)
            }
            private$update_help_url(help_port)
        },

        start_private_help_system = function(port=NULL){
            if(private$http_port == 0)
                self$start_httpd(port=port)
            private$help_port <- private$http_port
            private$help_url <- private$http_url
            em <- EventManager(type="http")
            em$activate()
        },
        start_help_system = function(){
            shared_help_system <- getOption("shared_help_system",NA)
            help_use_proxy <- getOption("help_use_proxy",NA)
            jupyterhub_prefix <- Sys.getenv("JUPYTERHUB_SERVICE_PREFIX")
            if(nzchar(jupyterhub_prefix)) {
                # We are likely in a multi-user environment
                if(is.na(shared_help_system))
                    shared_help_system <- FALSE
                if(is.na(help_use_proxy))
                    help_use_proxy <- TRUE
            } else {
                if(is.na(shared_help_system))
                    shared_help_system <- TRUE
                if(is.na(help_use_proxy))
                    help_use_proxy <- FALSE
            }
            private$shared_help_system <- shared_help_system
            private$help_use_proxy <- help_use_proxy
            
            if(shared_help_system)
                private$start_shared_help_system()
            else
                private$start_private_help_system()
            assign("get_help_url",private$get_help_url)
            assign("get_help_port",private$get_help_port)
            # log_out("start_help_system completed")
            # log_out(sprintf("help url: %s",private$get_help_url()))
        },
        shared_help_system = FALSE,
        jupyterhub_prefix = "",
        help_use_proxy = FALSE,

        get_help_url = function(){
            return(private$help_url)
        },
        get_help_port = function(){
            return(private$help_port)
        },

        new_cell = TRUE,

        add_payload = function(payload){
            private$payload <- append(private$payload,list(payload))
        },

        pager = function(files,header,title,delete.file){

            tmp_h <- character(length(files))
            tmp_h[] <- header
            header <- tmp_h
            
            if(length(title))
                text <- title
            else
                text <- character(0)
            
            for(i in seq_along(files)){
                file <- files[i]
                tmp <- readLines(file)
                tmp <- c(header[i],tmp)
                text <- c(text,tmp)
            }
            if(delete.file){
                file.remove(files)
            }
            text <- paste(text,collapse="\n")
            p <- Page("text/plain"=text)
            private$add_payload(unclass(p))
        },

        handle_eval = function(...) {
            # log_out("handle_eval")
            private$handle_text()
            private$handle_graphics()
        },

        handle_text = function(...) {
            text <- private$context$get_text()
            # log_out(sprintf("handle_text(%s)",text))
            # log_out("handle_text")
            # log_out(text,use.print=TRUE)
            if(nzchar(text)){
                text <- paste(text,collapse="\n")
                private$kernel$stream(text = text,
                                      stream = "stdout")
            }
        },

        last_plot_id = character(),


        handle_graphics = function(...) {

            if(!private$graphics$is_active()) return(NULL)
            plt <- private$graphics$get_plot()

            if(!length(plt) || !private$graphics$complete_page()) return(NULL)
            new_page <- private$graphics$new_page(reset=TRUE)
            private$last_plot <- private$current_plot
            if(new_page || plot_has_changed(current=plt,last=private$last_plot)) 
                private$current_plot <- plt
            else return(NULL)
            
            # log_out(sprintf("evaluator$handle_graphics(...,update=%s)",if(update)"TRUE"else"FALSE"))
            new_display <- new_page || private$new_cell && !getOption("jupyter.update.graphics",TRUE)
            
            width      <- getOption("jupyter.plot.width",6)
            height     <- getOption("jupyter.plot.height",6)
            pointsize  <- getOption("jupyter.plot.pointsize",12)
            resolution <- getOption("jupyter.plot.res",150)
            scale      <- getOption("jupyter.plot.scale",0.5)
            units      <- getOption("jupyter.plot.units","units")


            rkernel_graphics_types <- getOption("jupyter.graphics.types","image/png")

            mime_data <- list()
            mime_metadata <- list()

            for(mime in rkernel_graphics_types){
                mime_data[[mime]] <- mime_graphics(plt,
                                                   mime=mime,
                                                   width=width,
                                                   height=height,
                                                   pointsize=pointsize,
                                                   scale=scale,
                                                   res=resolution,
                                                   units=units)
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
            sleep_after_graphics <- getOption("sleep_after_graphics",0)
            if(sleep_after_graphics > 0)
                Sys.sleep(sleep_after_graphics)

        },

        handle_message = function(m) {
            # log_out("Evaluator: handle_message")
            text <- conditionMessage(m)
            text <- paste(text,collapse="\n")
            private$kernel$stream(text = text,
                           stream = "stdout")
        },

        handle_warning = function(w) {
            # log_out("Evaluator: handle_warning")
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
            private$kernel$log_error(text)
            stop_on_error <- getOption("rkernel_stop_on_error")
            if(stop_on_error){
                calls <- sys.calls()
                private$aborted <- TRUE
                #drop_prev <- private$nframes - 2
                #calls <- tail(calls,-drop_prev)
                calls <- head(calls,-6)
                calls <- tail(calls,-26)
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

        handle_result = function(x,visible) {
            # log_out("handle_result")
            # log_out(visible,use.print=TRUE)
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
                    if(nzchar(text)){
                        #private$kernel$stream(text = text,
                        #               stream = "stdout")
                        data <- list("text/plain"=text)
                        private$kernel$execute_result(data=data)
                    }
                }
                private$handle_graphics()
            }
        },

        handle_magic = function(magic,code,args){
            res <- dispatch_magic_handler(magic,code,args)
            if(length(res))
                private$handle_result(res,TRUE)
        },
        iframes = list(),
        handle_iframe = function(path,query,...){
            # log_out("handle_iframe: %s",path)
            path <- strsplit(path,"/")[[1]]
            log_out(path,use.print=TRUE)
            id <- path[3]
            body <- private$iframes[[id]]
            list(payload=body,
                 `content-type`="text/html",
                 headers=NULL,
                 `status code`=200)
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

        handle_str = function(){
            private$str_depth <- private$str_depth + 1
            if(private$str_depth == 1){
                em <- eventmanagers$output
                em$suspend("before_print")
                em$suspend("print")
                em$suspend("before_cat")
                em$suspend("cat")
            }
        },
        handle_str_exit = function(){
            if(private$str_depth == 1){
                private$handle_text()
                em <- eventmanagers$output
                em$activate("before_print")
                em$activate("print")
                em$activate("before_cat")
                em$activate("cat")
            }
            private$str_depth <- private$str_depth - 1
        },
        str_depth = 0,

        handle_context_enter = function(){
            private$graphics$activate()
        },
        handle_context_exit = function(){
            private$graphics$suspend()
        },

        get_shared_help_port = function(){
            port <- private$read_help_port_file()
            prefix <- private$jupyterhub_prefix
            if(port == 0){
                log_out("Port is 0 starting new help server")
                private$start_shared_help_server(port)
                while(port == 0){
                    port <- private$read_help_port_file()
                    Sys.sleep(0.01)
                }
                log_out(sprintf("New port number is %d",port))
                private$update_help_url(port)
            }
            return(port)
        },

        read_help_port_file = function(){
            user <- Sys.info()["user"]
            filename <- file.path(dirname(tempdir()),
                                  paste("RHelp",user,sep="-"))
            suppressWarnings(port <- tryCatch(readLines(filename),
                                              error=function(e) 0))
            as.integer(port)
        },

        publish_shared_help_port = function(port){
            user <- Sys.info()["user"]
            filename <- file.path(dirname(tempdir()),
                                  paste("RHelp",user,sep="-"))
            writeLines(as.character(port),filename)
        },
        
        start_shared_help_server = function(port){
            prefix <- private$jupyterhub_prefix
            use_proxy <- private$help_use_proxy
            arg_template <- "server <- RKernel::sharedHelpServer$new(%s,\"%s\",\"%s\"); server$run()"
            R_arg <- paste("-e",shQuote(sprintf(arg_template,port,prefix,use_proxy)))
            if(.Platform$OS.type == "windows") {
                R_binary <- file.path(R.home("bin"), "Rscript.exe")

            } else {
                R_binary <- file.path(R.home("bin"), "Rscript")
            }
            system2(R_binary,R_arg,wait=FALSE)
        },

        update_help_url = function(help_port){
            help_use_proxy <- private$help_use_proxy
            if(help_use_proxy){
                help_url <- sprintf("/proxy/%d",help_port)
                prefix <- private$jupyterhub_prefix
                if(nzchar(prefix)){
                    help_url <- paste0(prefix,help_url)
                    help_url <- gsub("//","/",help_url,fixed=TRUE)
                }
                private$help_port <- help_port
                private$help_url <- help_url
            } else {
                help_url <- sprintf("http://127.0.0.1:%d",help_port)
                private$help_port <- help_port
                private$help_url <- help_url
            }
        },

        assign_exports = function(){
            for(n in ls(exports)){
                obj <- get(n,envir=exports)
                assign(n,obj,envir=private$env)
            }
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

str2iframe <- function(...) evaluator$current$str2iframe(...)


