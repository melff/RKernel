DAPServer <- R6Class("DAPServer",
    public = list(
        r_session = NULL,
        r_send_request = NULL,
        send_debug_event = NULL,
        r_send_cmd = NULL,
        r_send_input = NULL,
        initialize = function(
                        r_session, 
                        r_send_request, 
                        send_debug_event, 
                        r_send_cmd,
                        r_send_input) {
            self$r_session <- r_session
            self$r_send_request <- r_send_request
            self$send_debug_event <- send_debug_event
            self$r_send_cmd <- r_send_cmd
            self$r_send_input <- r_send_input
        },
        handle = function(request, ...) {
            # log_out("DAPServer$handle")
            # if(request$command != "debugInfo"){
            #     log_out(sprintf("DAPServer: got command '%s'",request$command))
            #     log_out(request$arguments,use.print=TRUE)
            # }
            # else log_out(request$command)
            body <- switch(request$command,
                debugInfo = self$debugInfo(request, ...),
                initialize = self$debug_init(request, ...),
                attach = self$debug_attach(request, ...),
                disconnect = self$debug_disconnect(request, ...),
                inspectVariables = self$inspect_variables(request, ...),
                variables = self$reply_variables(request, ...),
                richInspectVariables = self$rich_inspect_variable(request, ...),
                dumpCell = self$dumpCell(request, ...),
                self$empty_reply(request, ...)
            )
            response <- list(
                type = "response",
                seq = self$response_seq,
                request_seq = request$seq,
                success = TRUE,
                command = request$command,
                body = body
            )
            self$response_seq <- self$response_seq + 1L
            # if(request$command != "debugInfo"){
            #     log_out("response:")
            #     log_out(response, use.print = TRUE)
            # }
            return(response)
        },
        response_seq = 1L,
        event = function(event, body = NULL) {
            # log_out("debug_event")
            content <- list(
                seq = self$event_seq,
                type = "event",
                event = event,
                body = body
            )
            # log_out(content, use.str=TRUE)
            self$send_debug_event(content)
            self$event_seq <- self$event_seq + 1L
        },
        event_seq = 1L,
        debugInfo = function(request){
            # log_out("debugInfo")
            list(
                isStarted = self$is_started,
                hashMethod = "Murmur2",
                hashSeed = 3339675911,
                tmpFilePrefix = tempdir(),
                tmpFileSuffix = ".R",
                breakpoints = list(),
                stoppedThreads = list(),
                richRendering = TRUE,
                exceptionPaths = list()
            )
        },    
        client_info = NULL,
        debug_init = function(request, ...) {
            self$client_info <- request$arguments
            list(
                supportsSetVariable = TRUE,
                supportsConditionalBreakpoints = TRUE,
                supportsConfigurationDoneRequest = TRUE,
                exceptionBreakpointFilters = list(
                    filter = "stop",
                    label = "Runtime error",
                    default = FALSE,
                    description = "Break when an error occurs that is not caught by a try() or tryCactch() expression"
                )
            )
        },
        is_started = FALSE,
        debug_attach = function(request) {
            # What I see with kernel spy and an ipython kernel
            # the following potentially relevant responses
            # debug_reply: type "response"
            # debug_event: event: "initialized"
            # debug_event: event: "process" - body: name (path to script), systemProcessId, isLocalProces, startMethod "attach"
            # debug_event: event: "thread" - body: reason: "started", threadId: (a number)
            #      6 thread events in total
            self$event(event = "initialized")
            self$event(
                event = "process",
                body = list(
                    systemProcessId = self$r_session$get_pid(),
                    name = Sys.which("R"),
                    isLocalProcess = TRUE,
                    startMethod = "attach"
                )
            )
            self$event(
                event = "thread",
                body = list(
                    reason = "started",
                    threadId = 1L
                )
            )
            list(
                supportsSetVariable = TRUE,
                supportsConditionalBreakpoints = TRUE,
                supportsConfigurationDoneRequest = TRUE,
                supportsFunctionBreakpoints = TRUE,
                supportsLogPoints = TRUE,
                exceptionBreakpointFilters = list(
                    filter = "stop",
                    label = "Runtime error",
                    default = FALSE,
                    description = "Break when an error occurs that is not caught by a try() or tryCactch() expression"
                )
            )
        },
        is_started = FALSE,
        debug_attach = function(request) {
            self$is_started <- TRUE
            return(NULL)
        },
        debug_disconnect = function(request) {
            self$is_started <- TRUE
            return(NULL)
        },
        inspect_variables = function(request) {
            variables <- self$r_send_cmd("RKernel::inspect_variables()")
            body <- list(variables = variables)
            return(body)
        },
        reply_variables = function(request) {
            # log_out("reply_variables")
            # log_out(request, use.str = TRUE)
            arguments <- request$arguments
            vref <- arguments$variablesReference
            cmd <- sprintf("RKernel:::ref2children(%d)", vref)
            variables <- self$r_send_cmd(cmd)
            body <- list(variables = variables)
            # log_out(body, use.str = TRUE)
            return(body)
        },
        dumpCell = function(request) {
            code <- request$arguments$code
            src_filename <- tempfile("codecell", fileext = ".R")
            writeLines(code, con = src_filename)
            # log_out(sprintf("dumped cell to '%s'", src_filename))
            list(
                sourcePath = src_filename
            )
        },
        rich_inspect_variable = function(request) {
            varname <- request$arguments$variableName
            cmd <- sprintf("RKernel::display(get0('%s'))", varname)
            response <- self$r_send_cmd(cmd)
            body <- response$content[c("data","metadata")]
            return(body)
        },
        empty_reply = function(request) {
            return(NULL)
        }
    )
)
