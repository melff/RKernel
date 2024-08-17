DAPServer <- R6Class("DAPServer",
  public = list(
      
      handle = function(request,...){
          # if(request$command != "debugInfo"){
          #     log_out(sprintf("DAPServer: got command '%s",request$command))
          #     log_out(request$arguments,use.print=TRUE)
          # }
          body <- switch(request$command,
                         debugInfo = self$debugInfo(request,...),
                         initialize = self$debug_init(request,...),
                         attach = self$debug_attach(request,...),
                         disconnect = self$debug_disconnect(request,...),
                         inspectVariables = self$inspect_variables(request,...),
                         variables = self$reply_variables(request,...),
                         richInspectVariables = self$rich_inspect_variable(request,...),
                         dumpCell = self$dumpCell(request,...),
                         self$empty_reply(request,...)
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
          return(response)
      },
      response_seq = 1L,
      event = function(event,body=NULL){
          
          content <- list(
              seq = self$event_seq,
              type = "event",
              event = event,
              body = body
          )
          msg_send(list(
              type = "debug_event",
              content = content
          ))
          self$event_seq <- self$event_seq + 1L
      },
      event_seq = 1L,
      client_info = NULL,
      debug_init = function(request,...){
          self$client_info <- request$arguments
          self$event(event="initialized")
          self$event(event="process",
                     body=list(
                         systemProcessId=Sys.getpid(),
                         name=Sys.which("R"),
                         isLocalProcess=TRUE,
                         startMethod="attach"
                     ))
          self$event(event="thread",
                     body=list(
                         reason="started",
                         threadId=1L
                     ))
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
      debug_attach = function(request,...){
          self$is_started <- TRUE
          return(NULL)
      },
      debug_disconnect = function(request,...){
          self$is_started <- TRUE
          return(NULL)
      },
      inspect_variables = function(request,envir,...){
          self$children <- list()
          body <- NULL
          varnames <- ls(envir)
          if(length(varnames)){
              variables <- lapply(varnames,self$inspect_thing,envir=envir)
              body <- list(variables = unname(variables))
          }
          return(body)
      },
      dumpCell = function(request,...){
          code = request$arguments$code
          src_filename <- tempfile("codecell",fileext=".R")
          writeLines(code,con=src_filename)
          log_out(sprintf("dumped cell to '%s'",src_filename))
          list(
              sourcePath = src_filename
          )
      },
      rich_inspect_variable = function(request,envir,...){
          varname <- request$arguments$variableName
          thing <- self$get_thing(varname,envir)
          # log_out(thing,use.str=TRUE)
          if(!length(thing$value))
              body <- display_data(data=list("text/html"=sprintf("Information from frontend: \"%s\"",request$arguments)))
          else 
              body <- display_data(thing$value)
          return(unclass(body))
      },
      inspect_thing = function(path,name=tail(path,n=1L),envir,...){
          desc <- self$get_thing(path,envir)
          x <- desc$value
          ename <- desc$ename
          vref <- 0L
          if(is.atomic(x)) {
              if(length(x) <= 1){
                  value <- format(x)
                  type <- class(x)
                  if(length(path) == 1)
                      type <- paste0(type," ",value)
                  else if(type %in% names(self$types)) 
                      type <- self$types[type]
              }
              else {
                  type <- self$get_atomic_type(x)
                  value <- self$get_atomic_val(x)
                  if(self$is.vectorlike(x)){
                      vref <- self$childref(path,start=1L)
                  }
              }
          }
          else if(is.function(x)){
              type <- "function"
              value <- "<function>"
              vref <- self$childref(path,start=1L)
          }
          else {
              type <- self$get_structured_type(x)
              value <- self$get_structured_val(x)
              vref <- self$childref(path,start=1L)
          }
          res <- list(
              name = name,
              value = value,
              type = type,
              evaluateName = ename,
              variablesReference = vref
          )
          return(res)
      },
      childref = function(path,start){
          vref <- self$path2vref(path=path,start=start)
          if(vref == 0){
              vref <- self$vref_counter
              self$children[[vref]] <- list(path=path,start=start)
              self$vref_counter <- vref + 1L
          }
          return(vref)
      },
      get_thing = function(path,envir){
          ename <- path[[1]]
          x <- get0(ename,envir=env)
          for(i in tail(path,-1)){
              if(i == "@"){
                  x <- attributes(x)
                  ename <- sprintf("attributes(%s)",ename)
              }
              else if(isS4(x)){
                  ename <- i
                  slotname <- gsub("@","",i,fixed=TRUE)
                  x <- slot(x,slotname)
              }
              else if(is.list(x)){
                  if(is.numeric(i) && i <= length(x) || is.character(i))
                      x <- x[[i]]
                  else return(NULL)
                  if(is.character(i))
                      ename <- paste0(ename,"$",i)
                  else
                      ename <- paste0(ename,"[[",i,"]]")
              }
              else {
                  x <- x[i]
                  ename <- paste0(ename,"[",i,"]")
              }
          }
          return(list(value=x,ename=ename,path=path))
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
      inspect_children = function(vref){
          desc <- self$children[[vref]]
          if(!length(desc)) return(NULL)
          path <- desc$path
          start <- desc$start
          thing <- self$get_thing(path)
          if(!length(thing)) return(NULL)
          ename <- thing$ename
          x <- thing$value
          l <- length(x)
          i0 <- desc$start
          if(is.list(x))
              m <- self$list_chunk_size
          else 
              m <- self$vector_chunk_size
          n <- min(m, l - i0 + 1L)
          ii <- seq.int(from=i0,
                        length=n)
          if(isS4(x)) self$show_S4(thing)
          else if(self$is.vectorlike(x)) self$show_vector(thing, ii,l)
          else if(is.list(x)) self$show_list(thing,ii,path,l)
          else if(is.function(x)) self$show_function(thing)
          else NULL
      },
      reply_variables = function(request){
          vref <- request$arguments$variablesReference
          list(variables=self$inspect_children(vref))
      },
      add_atts = function(thing){
          path <- thing$path
          ename <- thing$ename
          if(tail(path,1L)=="@") return(NULL) # Avoid infinite recursion
          att_path <- c(path,list("@"))
          vref <- self$childref(att_path,start=1L)
          list(list(
              name = "  - attributes",
              type = "str",
              value = "...",
              evaluateName = sprintf("attributes(%s)",ename),
              variablesReference = vref
          ))
      },
      more_elts = function(thing,n,l){
          path <- thing$path
          ename <- thing$ename
          vref <- self$childref(path,start=n)
          list(list(
                  name = paste0("[",n,":",l,"]"),
                  type = "str",
                  value = "...",
                  evaluateName = paste0(ename,"[",n,":",l,"]"),
                  variablesReference = vref
          ))
      },
      is.vectorlike = function(x){
          # A more 'inclusive' variant of 'is.vector'
          is.atomic(x) || (isS4(x) && is.vector(x@.Data))
      },
      show_vector = function(thing,ii,l){
          x <- thing$value
          ename <- thing$ename
          path <- thing$path
          nms <- names(x)
          if(!length(nms))
              nms <- paste0("[",ii,"]")
          zch <- !nzchar(nms)
          if(any(zch))
              nms[zch] <- paste0("[",ii[zch],"]")
          nms <- format(nms)
          vals <- format(x[ii])
          type <- class(x)[1]
          if(any(type %in% names(self$types))) {
              type <- intersect(type,names(self$types))
              type <- self$types[type]
          }
          else
              type <- "str" # To avoid NaN in the frontend
          enms <- paste0(ename,"[",ii,"]")
          elements <- Map(self$show_elt,vals,nms,enms,type)
          if(max(ii) < l){
              n <- max(ii) + 1L
              more_elts <- self$more_elts(thing,n,l)
              elements <- c(elements,more_elts)
          }
          if(length(attributes(x))){
              att_el <- self$add_atts(thing)
              elements <- c(att_el,elements)
          }
          unname(elements)
      },
      show_list = function(thing,ii,path,l){
          x <- thing$value
          ename <- thing$ename
          nms <- names(x)
          if(!length(nms))
              nms <- paste0("[[",ii,"]]")
          zch <- !nzchar(nms)
          if(any(zch))
              nms[zch] <- paste0("[[",ii[zch],"]]")
          nms <- format(nms)
          vals <- x[ii]
          enms <- paste0(ename,"[[",ii,"]]")
          elements <- list()
          for(i in ii){
              elements[[i]] <- self$inspect_thing(c(path,list(i)),name=nms[i])
          }
          if(max(ii) < l){
              n <- max(ii) + 1L
              more_elts <- self$more_elts(thing,n,l)
              elements <- c(elements,more_elts)
          }
          if(length(attributes(x))){
              att_el <- self$add_atts(thing)
              elements <- c(att_el,elements)
          }
          unname(elements)
      },
      show_S4 = function(thing){
          x <- thing$value
          ename <- thing$ename
          path <- thing$path
          nms <- paste0("@",slotNames(x))
          ii <- seq_along(nms)
          elements <- list()
          for(i in ii){
              n <- nms[i]
              elements[[i]] <- self$inspect_thing(c(path,list(n)),name=n)
          }
          unname(elements)
      },
      show_function = function(thing,...){
          f <- thing$value
          args <- format(formals(f))
          vals <- unname(args)
          vals[!nzchar(vals)] <- "<no default>"
          nms <- names(args)
          elements <- Map(self$show_elt,vals,nms,"","str")
          unname(elements)
      },
      show_elt = function(x,name,ename,type){
          list(name=name,
               value=x,
               type=type,
               evaluateName=ename,
               variablesReference=0L)
      },
      paths_equal = function(path1,path2){
          path1 <- paste0(unlist(path1),collapse="_")
          path2 <- paste0(unlist(path2),collapse="_")
          path1 == path2
      },
      path2vref = function(path,start=1L){
          for(i in seq_along(self$children)){
              child <- self$children[[i]]
              if(self$paths_equal(path,child$path) && start==child$start) return(i)
          }
          return(0)
      },
      is_started = FALSE,
      types = c(
          "integer"="int",
          "logical"="bool",
          "numeric"="float",
          "character"="str",
          "factor"="str"
      ),
      vref_counter = 1L,
      children = list(),
      vector_chunk_size = 10L,
      list_chunk_size = 100L
  )
)

dap_server <- new.env()
dap_server$current <- NULL

get_dap_server <- function(){
    if(is.null(dap_server$current))
        dap_server$current <- DAPServer$new()
    dap_server$current
}
