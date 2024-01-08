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
                         variables = self$reply_variables(request),
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
              if(length(var) == 1){
                  value <- format(var)
                  type <- typeof(var)
                  type <- paste0(type," ",value)
              }
              else {
                  type <- self$get_atomic_type(var)
                  value <- self$get_atomic_val(var)
                  if(is.vector(var)){
                      vref <- self$vref_counter
                      self$vref_counter <- self$vref_counter + 1L
                      self$var_children[[vref]] <- list(name=varname,envir=envir,start_index=1L)
                  }
              }
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
      reply_variables = function(request){
          vref <- request$arguments$variablesReference
          children_desc <- self$var_children[[vref]]
          if(!length(children_desc))
              return(NULL)
          e <- children_desc$envir
          varname <- children_desc$name
          var <- get0(varname,e)
          if(!length(var)) return(NULL)
          l <- length(var)
          i0 <- children_desc$start_index
          n <- min(self$chunk_size, l - i0 + 1L)
          ii <- seq.int(from=i0,
                        length=n)
          if(is.atomic(var) && is.vector(var)) self$show_vector(var, ii, varname, l)
          else return(NULL)
      },
      show_vector = function(x,ii,name,l){
          nms <- names(x)
          if(!length(nms))
              nms <- paste0("[",ii,"]")
          zch <- !nzchar(nms)
          if(any(zch))
              nms[zch] <- paste0("[",ii[zch],"]")
          nms <- format(nms)
          vals <- format(x[ii])
          type <- typeof(x) 
          if(type %in% names(self$types)) 
              type <- self$types[type]
          enms <- paste0(name,"[",ii,"]")
          variables <- Map(self$show_elt,vals,nms,enms,type)
          if(max(ii) < l){
              n <- max(ii) + 1L
              more_elts <- list(
                  name = paste0("[",n,":",l,"]"),
                  type = "str",
                  value = "...",
                  evaluateName = paste0(name,"[",n,":",l,"]"),
                  variablesReference=0L
              )
              variables <- c(variables,list(more_elts))
          }
          list(variables=unname(variables))
      },
      show_elt = function(val,name,ename,type){
          list(name=name,
               value=val,
               type=type,
               evaluateName=ename,
               variablesReference=0L)
      },
      is_started = FALSE,
      kernel = NULL,
      envir = NULL,
      types = c(
          "integer"="int",
          "logical"="bool",
          "double"="float",
          "character"="str"
      ),
      vref_counter = 1L,
      var_children = list(),
      chunk_size = 25L
  )
)
