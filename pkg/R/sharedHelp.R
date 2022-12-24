#' @export
sharedHelpServer <- R6Class("sharedHelpServer",
   public = list(
       port = 0,
       url = "",
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
       httpd = function(path,query,...){
           response <- self$orig_httpd(path=path,query=query,...)
           payload <- response$payload
           payload <- gsub("/doc/html/",paste0(self$url,"/doc/html/"),
                           payload,fixed=TRUE)
           response$payload <- payload
           return(response)
       },
       orig_httpd = NULL,
       run = function(){
           repeat Sys.sleep(3600)
       },
       publish_port = function(port){
            user <- Sys.info()["user"]
            filename <- file.path(dirname(tempdir()),
                                  paste("RHelp",user,sep="-"))
            writeLines(as.character(port),filename)
       },
       log = function(text){
            user <- Sys.info()["user"]
            filename <- file.path(dirname(tempdir()),
                                  paste("RHelp",user,sep="-"))
            filename <- paste0(filename,".log")
            writeLines(text,filename)
       }
  ) 
)
