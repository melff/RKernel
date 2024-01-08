DAPServer <- R6Class("DAPServer",
  public = list(
      initialize = function(kernel){
          self$kernel <- kernel
          self$envir <- globalenv()
      },
      handle = function(request){
          if(request$command != "debugInfo")
              log_out(sprintf("DAPServer: got command '%s",request$command))
          body <- switch(request$command,
                         debugInfo = self$debugInfo(request),
                         initialize = self$debug_init(request),
                         attach = self$debug_attach(request),
                         # setExceptionBreakpoints = self$empty_reply(request),
                         inspectVariables = self$inspect_variables(request),
                         # configurationDone = self$empty_reply(request),
                         # variables = self$empty_reply(request),
                         self$empty_reply(request)
                         )
          response <- list(
              type = "response",
              request_seq = request$seq,
              success = TRUE,
              command = request$command,
              body = body
          )
          if(request$command != "debugInfo")
              log_out(response,use.print=TRUE)
          return(response)
      },
      debugInfo = function(request){
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
      debug_init = function(request){
          list(
              supportsSetVariable = TRUE
          )
      },
      debug_attach = function(request){
          self$is_started <- TRUE
          return(NULL)
      },
      inspect_variables = function(request){
          body <- NULL
          e <- self$envir
          varnames <- ls(e)
          if(length(varnames)){
              variables <- lapply(varnames,self$inspect_variable,envir=e)
              body <- list(variables = unname(variables))
          }
          return(body)
      },
      inspect_variable = function(varname,envir){
          var <- get(varname,envir=envir)
          log_out(var,use.str=TRUE)
          vref <- 0L
          if(is.atomic(var)) {
              type <- self$get_atomic_type(var)
              value <- self$get_atomic_val(var)
          }
          else {
              type <- self$get_structured_type(var)
              value <- self$get_structured_val(var)
          }
          res <- list(
              name = varname,
              value = value,
              type = type,
              evaluateName = varname,
              variablesReference = vref
          )
          return(res)
      },
      get_atomic_type = function(x){
          gsub("NULL ...","",capture.output(str(x,vec.len=0L,give.attr=FALSE)))
      },
      get_atomic_val = function(x){
          capture.output(str(x,give.head=FALSE,give.attr=FALSE))
      },
      get_structured_type = function(x){
          capture.output(str(x,list.len=FALSE))[1]
      },
      get_structured_val = function(x){
          "<list or object>"
      },
      empty_reply = function(request){
          #log_info("Unhandled debug command")
          #log_info(request,use.print=TRUE)
          return(NULL)
      },
      is_started = FALSE,
      kernel = NULL,
      envir = NULL
  )
)
