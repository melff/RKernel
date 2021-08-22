#' @importFrom evaluate evaluate new_output_handler
#' @importFrom utils capture.output
#' @importFrom svglite svgstring
#' @importFrom evaluate parse_all
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

        initialize = function(kernel){
            private$kernel <- kernel
        },

        startup = function(...) {

            self$env <- new.env()
            attach(self$env,name="RKernel")
            pos <- match("RKernel",search())
            assign("q",self$quit,pos=pos)
            assign("quit",self$quit,pos=pos)
            assign("cell.options",self$cell.options,pos=pos)
            assign("cell.par",self$cell.par,pos=pos)

            assign("display",display,pos=pos)
            assign("Page",Page,pos=pos)

            assign("add_paged_classes",add_paged_classes,pos=pos)
            assign("add_displayed_classes",add_displayed_classes,pos=pos)

            if("var_dic_list" %in% objects(envir=.GlobalEnv)) 
                rm(var_dic_list,envir=.GlobalEnv)
            assign("var_dic_list",self$var_dic_list,pos=pos)

            options(device=dummy_device,
                    pager=self$pager,
                    crayon.enabled=TRUE,crayon.colors=256L,
                    jupyter.graphics.types=c("image/svg+xml","image/png","application/pdf"),
                    jupyter.plot.width=6,
                    jupyter.plot.height=6,
                    jupyter.plot.pointsize=12,
                    jupyter.plot.res=96,
                    jupyter.plot.units="in",
                    jupyter.embed.graphics=TRUE)

            #options(jupyter.graphics.types=c("image/png","application/pdf"))

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

            setHook('plot.new',self$plot_new_hook)
            setHook('grid.newpage',self$plot_new_hook)
            setHook('before.plot.new',self$before_plot_new_hook)
            setHook('before.grid.newpage',self$before_plot_new_hook)
            
            suppressMessages(trace(plot.xy,self$plot_xy_hook,print=FALSE))
            suppressMessages(trace(curve,quote(curve_hook(add)),print=FALSE))

            assign("curve_hook",self$curve_hook,pos=pos)
            add_paged_classes(c("help_files_with_topic","packageIQR"))
            add_displayed_classes(c("htmlwidget"))

            private$comm_dispatcher <- private$kernel$comm_dispatcher
        },

        shutdown = function(){
            base::q()
        },

        plot_new_called = FALSE,

        current_plot = list(),

        plot_new_hook = function(...){
            self$plot_new_called <- TRUE
        },

        graphics_par_usr = numeric(0),

        curve_hook = function(add){
            if(isTRUE(add) && !self$plot_new_called && 
               length(self$current_plot) > 0){
                replayPlot(self$current_plot)
                par(usr = self$graphics_par_usr)
                self$plot_new_called <- TRUE
            }
        },

        plot_xy_hook = function(...){
            if(!self$plot_new_called && 
               length(self$current_plot) > 0){
                replayPlot(self$current_plot)
                par(usr = self$graphics_par_usr)
                self$plot_new_called <- TRUE
            }
        },

        eval = function(code,...,silent=FALSE){

            if("var_dic_list" %in% objects(envir=.GlobalEnv)) 
                rm(var_dic_list,envir=.GlobalEnv)
            #pos <- match("RKernel",search())
            #assign("var_dic_list",self$var_dic_list,pos=pos)

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
                self$plot_new_called <- FALSE
                tryCatch(
                    evaluate(code,
                             envir=.GlobalEnv,
                             stop_on_error=1L,
                             output_handler=self$output_handlers$default,
                             new_device=TRUE),
                    interrupt = self$handle_interrupt)
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
            self$current_plot <- plt
            self$graphics_par_usr <- par("usr")

            width <- getOption("jupyter.plot.width",6)
            height <- getOption("jupyter.plot.height",6)
            pointsize <- getOption("jupyter.plot.pointsize",12)
            resolution <- getOption("jupyter.plot.res",96)
            embedded <- getOption("jupyter.embed.graphics",TRUE)

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
                    width=width*resolution,
                    height=height*resolution)
                if(mime=="image/svg+xml")
                    mime_metadata[[mime]]$isolated <-TRUE
            }

            if(embedded){
                result <- list(
                    display_data = list(
                        data = mime_data,
                        metadata = mime_metadata
                    )
                )
                self$add_result(result)
            } else if("image/svg+xml" %in% rkernel_graphics_types){
                payload <- list(
                    source="page",
                    data=list(
                        "text/html"=mime_data[["image/svg+xml"]]
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
                text <- paste0("\nWarning:\n\t",paste(text,collapse="\n"))
            } else {
                call <- deparse(call)[[1]]
                text <- paste0("\nWarning in ",call,":\n\t",paste(text,collapse="\n"))
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
            stop_on_error <- getOption("rkernel_stop_on_error",TRUE)
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
            self$status <- "aborted"
            self$aborted <- TRUE
        },

        set_last_value = function(x){
            pos <- match("RKernel",search())
            assign(".Last.value",x,pos=pos)
        },

        code_is_complete = function(code){
            status <- tryCatch({
                parse_all(code)
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
        }

    ),
    
    private = list(
        kernel=list(),
        comm_dispatcher=list()
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

dummy_dev_filename <- function(...) file.path(tempdir(),"dummy-device.png")

#' @importFrom grDevices png
dummy_device <- function(filename = dummy_dev_filename(),
                         width = getOption("jupyter.plot.width",7),
                         height = getOption("jupyter.plot.height",7),
                         res = getOption("jupyter.plot.res",96),
                         pointsize = getOption("jupyter.plot.pointsize",12),
                         units = getOption("jupyter.plot.units","in"),
                         ...) png(filename,
                                  width=width,
                                  height=height,
                                  res=res,
                                  units=units,...)

splitLines <- function(text) strsplit(text,"\n",fixed=TRUE)[[1]]


