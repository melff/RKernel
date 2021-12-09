#' @importFrom utils capture.output
#' @importFrom svglite svgstring
#' @importFrom grDevices png

OutputWatcher <- R6Class("OutputWatcher",
  public = list(
      connection = NULL,
      last_plot = NULL,
      current_plot = NULL,
      text_callback = NULL,
      graphics_callback = NULL,
      text_output = NULL,
      prev_text_output = NULL,
      initialize = function(text_callback=NULL,graphics_callback=NULL){
          self$text_callback <- text_callback
          self$graphics_callback <- graphics_callback
          self$connection <- textConnection(NULL,"wr",local=TRUE)
          sink(self$connection,split=FALSE)
          self$init_graphics()
      },
      open = function(){
          self$connection <- textConnection(NULL,"wr",local=TRUE)
          sink(self$connection,split=FALSE)
      },
      close = function(){
          sink()
          close(self$connection)
      },
      handle_output = function(){
          # log_out("OutputWatcher$handle_output ")
          # log_out(self$connection,use.print=TRUE)
          # log_out(text_output,use.print=TRUE)
          self$handle_text()
          self$handle_graphics()
        },

      handle_text = function(new_line=FALSE){
          # log_out("OutputWatcher$handle_text")
          if(isIncomplete(self$connection) || new_line)
              cat("\n",file=self$connection)
          self$prev_text_output <- self$text_output
          self$text_output <- textConnectionValue(self$connection)
          if(is.function(self$text_callback)){
              nlines <- length(self$prev_text_output)
              if(nlines > 0)
                  current_text_output <- tail(self$text_output,-nlines)
              else
                  current_text_output <- self$text_output
              if(length(current_text_output)){
                  current_text_output <- paste(current_text_output,collapse="\n")
                  # log_out(paste(current_text_output))
                  self$text_callback(current_text_output)
              }
          }
      },

      graphics_active = function(){
          res <- self$dev_num > 1 && self$dev_num == dev.cur()
          # if(res) log_out("graphics is active")
          # else log_out("graphics not active")
          return(res)
      },

      handle_graphics = function(){
          # log_out("OutputWatcher$handle_graphics")
          if(self$graphics_active()){
              self$last_plot <- self$current_plot
              plt <- recordPlot()
              # log_out("taking graphics snapshot")
              # log_out(sprintf("plot_new_called = %s",if(self$plot_new_called)"TRUE"else"FALSE"))
              do_send_plot <- !plot_is_empty(plt) && !identical(self$last_plot,plt) 
              if(do_send_plot) {
                  # log_out("graphics_callback")
                  update <- !self$plot_new_called
                  self$graphics_callback(plt,update=update)
                  self$current_plot <- plt
                  self$plot_new_called <- FALSE
                  # log_out("Setting plot_new_called <- FALSE")
              }
          }
      },

      plot_new_called = FALSE,
      graphics_par_usr = numeric(0),

      before_plot_new_hook = function(...){
          # log_out("before_plot_new_hook")
          if(self$graphics_active()){
              self$handle_text()
          }
      },

      plot_new_hook = function(...){
          # log_out("plot_new_hook")
          if(self$graphics_active()){
              self$plot_new_called <- TRUE
              # log_out("Setting plot_new_called <- TRUE")
              self$graphics_par_usr <- par("usr")
          }
      },

      plot_xy_hook = function(...){
          # log_out("plot_xy_hook")
          if(self$graphics_active()){
              if(FALSE && !self$plot_new_called){
                  par(usr = self$graphics_par_usr)
                  replayPlot(self$current_plot)
                  self$plot_new_called <- TRUE
                  # log_out("Setting plot_new_called <- TRUE")
              } 
          }
      },

      dev_filename = character(0),
      dev_name = character(),
      dev_num = 0,
      device = list(),

      init_graphics = function(){
          os <- .Platform$OS.type
          sysname <- Sys.info()[["sysname"]]
          if(os == "unix" && sysname=="Darwin")
              os <- "osx"
          self$dev_filename <- switch(os,
                                      windows="NUL",
                                      osx=NULL,
                                      unix="/dev/null")
          self$dev_name <- switch(os,
                                  windows="png",
                                  osx="pdf",
                                  unix="png")
          self$device <- function(filename = NULL,
                                 width = getOption("jupyter.plot.width",6),
                                 height = getOption("jupyter.plot.height",6),
                                 res = getOption("jupyter.plot.res",96),
                                 pointsize = getOption("jupyter.plot.pointsize",12),
                                 units = getOption("jupyter.plot.units","in"),
                                 ...){
              dev <- get(self$dev_name)
              if(is.null(filename))
                  dev(filename=self$dev_filename,
                      width=width,
                      height=height,
                      res=res,
                      units=units,
                      ...)
              self$dev_num <- dev.cur()
              dev.control(displaylist="enable")
          }
          options(device=self$device)

          setHook('plot.new',self$plot_new_hook)
          setHook('grid.newpage',self$plot_new_hook)
          setHook('before.plot.new',self$before_plot_new_hook)
          setHook('before.grid.newpage',self$before_plot_new_hook)

          suppressMessages(trace(plot.xy,self$plot_xy_hook,print=FALSE))
          suppressMessages(trace(arrows,self$plot_xy_hook,print=FALSE))
          suppressMessages(trace(segments,self$plot_xy_hook,print=FALSE))
          suppressMessages(trace(abline,self$plot_xy_hook,print=FALSE))
          suppressMessages(trace(box,self$plot_xy_hook,print=FALSE))
          suppressMessages(trace(rect,self$plot_xy_hook,print=FALSE))
          suppressMessages(trace(polygon,self$plot_xy_hook,print=FALSE))
          suppressMessages(trace(dev.off,self$dev_off_hook,print=FALSE))

      },
      dev_off_hook = function(){
          closed_dev <- get("which",envir=parent.frame())
          if(closed_dev == self$dev_num)
              self$dev_num <- NULL
      },
      new_expression = function() {
          self$plot_new_called <- FALSE
      }
  )                      
)

# cf. 'graphics.r' in package "evaluate" (Yihui Xie et al.)
empty_plot_calls <- c("palette",
                      "palette2",
                      paste0("C_",c("layout",
                                  "par",
                                  "clip",
                                  "strWidth",
                                  "strHeight",
                                  "plot_new",
                                  "plot_window")))

plot_is_empty <- function(plt) {
    if(!length(plt)) return(TRUE)
    pcalls <- plot_calls(plt)
    # log_out(pcalls,use.print=TRUE)
    # log_out(empty_plot_calls,use.print=TRUE)
    # log_out(pcalls%in%empty_plot_calls,use.print=TRUE)
    if(!length(pcalls)) return(TRUE)
    res <- all(pcalls %in% empty_plot_calls)
    # log_out(res)
    return(res)
}

plot_calls <- function(plt){
    plt <- plt[[1]]
    plt <- lapply(plt,"[[",2)
    if(!length(plt)) return(NULL)
    plt <- lapply(plt,"[[",1)
    sapply(plt,get_name_el)
}

get_name_el <- function(x){
    if(length(x$name)) x$name 
    else deparse(x)
}


Eval <- function(expressions,
                  error_handler,
                  warning_handler,
                  message_handler,
                  value_handler,
                  watcher){
    # log_out("Eval")
    n <- length(expressions)
    if(n < 1) return(NULL)
    mHandler <- function(m) {
        message_handler(m)
        invokeRestart("muffleMessage")
    }
    wHandler <- function(w){
        if (getOption("warn") >= 2) return()
        warning_handler(w)
        invokeRestart("muffleWarning")
    }
    eHandler <- function(e) {
        error_handler(e)
        # invokeRestart("muffleError")
    }
     
    watcher$new_expression()
    for(i in 1:n){
        # log_out(sprintf("exressions[[%d]]",i))
        expr <- expressions[[i]]
        ev <- list(value = NULL, visible = FALSE)
        # log_out("evaluating ...")
        ### See 'evaluate_call' in package "evaluate" (Yihui Xie et al.)
        try(ev <- withCallingHandlers(
                withVisible(eval(expr,envir=.GlobalEnv)),
                error=eHandler,
                warning=wHandler,
                message=mHandler),silent=TRUE)
        # log_out("handling output ...")
        watcher$handle_output()
        try(ev <- withCallingHandlers(
                value_handler(ev$value,ev$visible),
                error=eHandler,
                warning=wHandler,
                message=mHandler),silent=TRUE)
        watcher$handle_output()
        reset <- FALSE
    }
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

        watcher = list(),

        initialize = function(kernel){
            private$kernel <- kernel
        },

        startup = function(...) {

            self$env <- new.env()
            attach(self$env,name="RKernel")
            pos <- match("RKernel",search())
            assign("q",self$quit,pos=pos)
            assign("quit",self$quit,pos=pos)
            # assign("cell.options",self$cell.options,pos=pos)
            # assign("cell.par",self$cell.par,pos=pos)
            # 
            # # assign("display",self$display,pos=pos)
            # assign("display",display,pos=pos)
            # assign("stream",self$stream,pos=pos)
            # #assign("cat",self$cat,pos=pos)
            # #assign("print",self$print,pos=pos)
            # 
            # assign("Javascript",Javascript,pos=pos)
            # assign("Math",LaTeXMath,pos=pos)
            # assign("raw_html",raw_html,pos=pos)
            # assign("Page",Page,pos=pos)
            # assign("View",View,pos=pos)
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

            self$watcher <- OutputWatcher$new(text_callback=self$handle_text,
                                              graphics_callback=self$handle_graphics)
            
            self$start_help_system()
            assign("help_proc",self$help_proc,pos=pos)
            assign("help_port",self$help_port,pos=pos)
            assign("help.start",self$help_start,pos=pos)

            assign("get_help_url",function()self$help_url,pos=pos)

        },

        source_depth = 0,
        source_entry_hook = function(){
            if(self$source_depth < 1)
                suppressMessages(trace(cat,
                                      self$watcher$handle_graphics,
                                      print=FALSE))
            self$source_depth <- self$source_depth + 1
        },
        source_exit_hook = function(){
            self$source_depth <- self$source_depth - 1
            if(self$source_depth < 1)
                suppressMessages(untrace(cat))
        },
        print_exit_hook = function(){
            # log_out("print_exit_hook")
            self$watcher$handle_text(new_line=TRUE)
        },
        cat_exit_hook = function(){
            # log_out("cat_exit_hook")
            self$watcher$handle_text()
        },


        help_port = integer(),
        help_proc = integer(),
        help_url = character(),

        start_help_system = function(help_port=NULL){
            if(is.null(help_port)){
                repeat{
                    help_port <- tools::startDynamicHelp(TRUE)
                    if(help_port > 0) break
                }
                port0 <- tools::startDynamicHelp(FALSE)
                stopifnot(port0 == 0)
            }
            help_port <- as.integer(help_port)
            self$help_proc  <- callr::r_bg(function(port){
                options(help.ports=port)
                tools::startDynamicHelp(TRUE)
                repeat Sys.sleep(60)
            },args=list(port=help_port))
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

        eval = function(code,...,silent=FALSE){
            # log_out("evaluator$eval")
            suppressMessages(trace(source,
                                   self$source_entry_hook,
                                   exit=self$source_exit_hook,
                                   print=FALSE))
            suppressMessages(trace(print,
                                   exit=self$print_exit_hook,
                                   print=FALSE))
            suppressMessages(trace(cat,
                                   exit=self$cat_exit_hook,
                                   print=FALSE))
            on.exit({
                suppressMessages(untrace(cat))
                suppressMessages(untrace(print))
                suppressMessages(untrace(source))
            })
            
            
            perc_match <- getMatch(code,regexec("^%%(.+?)\n\n",code))
            if(length(perc_match) > 1){
                magic <- perc_match[2]
                # message(sprintf("Found magic '%s'",magic))
                code <- gsub("^%%.+?\n\n","",code)
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
                Eval(expressions,
                     error_handler=self$handle_error,
                     warning_handler=self$handle_warning,
                     message_handler=self$handle_message,
                     value_handler=self$handle_value,
                     watcher=self$watcher
                     )

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
            #log_out("handle_text")
            #log_out(text,use.print=TRUE)
            text <- paste(text,collapse="\n")
            private$kernel$stream(text=text,stream="stdout")
        },

        last_plot_id = character(),

        handle_graphics = function(plt,update=FALSE) {

            update <- update && getOption("jupyter.update.graphics",TRUE)
            # log_out(sprintf("evaluator$handle_graphics(...,update=%s)",if(update)"TRUE"else"FALSE"))

            width <- getOption("jupyter.plot.width")
            height <- getOption("jupyter.plot.height")
            pointsize <- getOption("jupyter.plot.pointsize")
            resolution <- getOption("jupyter.plot.res")
            scale <- getOption("jupyter.plot.scale")

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

            if(update){
                id <- self$last_plot_id
                cls <- "update_display_data"
            } 
            else {
                id <- UUIDgenerate()
                self$last_plot_id <- id
                cls <- "display_data"
            } 

            d <- list(data = mime_data,
                      metadata = mime_metadata,
                      transient = list(display_id=id))
            class(d) <- cls

            # log_out(str(d),use.print=TRUE)

            channel <- get_current_channel()
            channel$display_send(d)

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
            if(magic == "Math"){
                d <- LaTeXMath(code)
                private$kernel$display_data(data=d$data,
                                            metadata=d$metadata,
                                            transient=d$transient)
            } 
            else if (magic == "Javascript"){
                d <- Javascript(code)
                private$kernel$display_data(data=d$data,
                                            metadata=d$metadata,
                                            transient=d$transient)
            }
            else if (magic == "html"){
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

        # display = function(...){
        #     d <- display(...)
        #     private$kernel$display_data(data=d$data,
        #                                 metadata=d$metadata,
        #                                 transient=d$transient)
        # },

        stream = function(text, stream=c("stdout","stderr")){
            stream <- match.arg(stream)
            private$kernel$stream(text=text,
                                  stream=stream)
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

        init_scrolling_table = function(){
            d <- init_scrolling_table()
            private$kernel$display_data(d$data)
        },

        init_env_browser = function(){
            d <- init_env_browser()
            private$kernel$display_data(d$data)
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


splitLines <- function(text) strsplit(text,"\n",fixed=TRUE)[[1]]

