#' @title R6 objects that run a help server
#'
#' @description An Object of class "sharedHelper" serves HTML help pages for one or several
#'   RKernel processes
#' @export
sharedHelpServer <- R6Class("sharedHelpServer",
   public = list(
       #' @field port Integer, the port number
       port = 0,
       #' @field url Character string, the base URL of the served help pages
       url = "",
       #' @description
       #' Intialize the object
       #' @param port Integer, a port number
       #' @param prefix A character string, the URL prefix
       #' @param use_proxy A logical value, whether the help server is supposed to be run
       #'   behind a jupyter proxy
       initialize = function(port=0,prefix="",use_proxy=FALSE){
           if(port > 10000)
               options(help.ports=port)
           else
               options(help.ports=NULL)
           help_ports <- getOption("help.ports")
           if(length(help_ports)){
               help_ports <- paste(help_ports,collapse=", ")
               self$log(sprintf("Trying ports: %s",help_ports))
           }
           help_port <- tryCatch(tools::startDynamicHelp(TRUE),
                                 error=function(e){
                                     self$log(e$message)
                                     return(0)
                                 })
           self$log(sprintf("Obtained port: %s",help_ports))
           if(use_proxy){
               url <- sprintf("/proxy/%d",help_port)
               if(nzchar(prefix)){
                   url <- paste0(prefix,url)
                   url <- gsub("//","/",url,fixed=TRUE)
               }
               self$url <- url
               self$orig_httpd <- tools:::httpd
               replace_in_package("tools","httpd",self$httpd)
           }
           self$publish_port(help_port)
       },
       #' @description
       #' The function that serves paths and queries
       #' @param path A character string, the path part of an URL
       #' @param query An optional HTTP query string
       #' @param ... Any other arguments, passed on to the original 'httpd' function.
       httpd = function(path,query,...){
           response <- self$orig_httpd(path=path,query=query,...)
           payload <- response$payload
           payload <- gsub("/doc/html/",paste0(self$url,"/doc/html/"),
                           payload,fixed=TRUE)
           response$payload <- payload
           return(response)
       },
       #' @field orig_httpd A function, set to the original HTTP server function
       #   from the "tools" package, after initialisation of the object
       orig_httpd = NULL,
       #' @description
       #' The server loop 
       run = function(){
           repeat Sys.sleep(3600)
       },
       #' @description
       #' Put a port number into a temporary file, for other processes to find
       #' @param port An integer, the port number
       publish_port = function(port){
            user <- Sys.info()["user"]
            filename <- file.path(dirname(tempdir()),
                                  paste("RHelp",user,sep="-"))
            writeLines(as.character(port),filename)
       },
       #' @description
       #' Put log text into a temporary file, for other processes to read
       #' @param text A character string to be added to the log file
       log = function(text){
            user <- Sys.info()["user"]
            filename <- file.path(dirname(tempdir()),
                                  paste("RHelp",user,sep="-"))
            filename <- paste0(filename,".log")
            writeLines(text,filename)
       }
  ) 
)
