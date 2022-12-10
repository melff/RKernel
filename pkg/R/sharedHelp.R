#' @export
sharedHelpServer <- R6Class("sharedHelpServer",
   public = list(
       port = 0,
       url = character(0),
       initialize = function(port,url=""){
           options(help.ports=port)
           help_port <- tools::startDynamicHelp(TRUE)
           self$url <- url
           self$orig_httpd <- tools:::httpd
           replace_in_package("tools","httpd",self$httpd)
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
       }
  ) 
)
