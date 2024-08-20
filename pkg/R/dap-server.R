DAPServer <- R6Class("DAPServer",
    public = list(
        r_session = NULL,
        r_send_request = NULL,
        send_debug_event = NULL,
        r_send_cmd = NULL,
        initialize = function(r_session, r_send_request, send_debug_event, r_send_cmd) {
            self$r_session <- r_session
            self$r_send_request <- r_send_request
            self$send_debug_event <- send_debug_event
            self$r_send_cmd <- r_send_cmd
        },
        handle = function(request, ...) {
            log_out("DAPServer$handle")
            # if(request$command != "debugInfo"){
            #     log_out(sprintf("DAPServer: got command '%s",request$command))
            #     log_out(request$arguments,use.print=TRUE)
            # }
            log_out(request$command)
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
            log_out("response:")
            log_out(response, use.print = TRUE)
            return(response)
        },
        response_seq = 1L,
        event = function(event, body = NULL) {
            content <- list(
                seq = self$event_seq,
                type = "event",
                event = event,
                body = body
            )
            self$send_debug_event(content)
            self$event_seq <- self$event_seq + 1L
        },
        event_seq = 1L,
        debugInfo = function(request){
            log_out("debugInfo")
            log_out(self$is_started)
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
        debug_attach = function(request, ...) {
            self$is_started <- TRUE
            return(NULL)
        },
        debug_disconnect = function(request, ...) {
            self$is_started <- TRUE
            return(NULL)
        },
        inspect_variables = function(request, envir, ...) {
            variables <- self$r_send_cmd("RKernel::inspect_variables()")
            log_out(variables, use.str = TRUE)
            body <- list(variables = variables)
            return(body)
        },
        dumpCell = function(request, ...) {
            code <- request$arguments$code
            src_filename <- tempfile("codecell", fileext = ".R")
            writeLines(code, con = src_filename)
            log_out(sprintf("dumped cell to '%s'", src_filename))
            list(
                sourcePath = src_filename
            )
        },
        rich_inspect_variable = function(request, envir, ...) {
            varname <- request$arguments$variableName
            thing <- self$get_thing(varname, envir)
            # log_out(thing,use.str=TRUE)
            if (!length(thing$value)) {
                body <- display_data(
                    data = list("text/html" = sprintf(
                        "Information from frontend: \"%s\"", request$arguments
                    ))
                )
            } else {
                body <- display_data(thing$value)
            }
            return(unclass(body))
        }
    )
)
