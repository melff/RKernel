#' @include widget.R env-browser.R

envBrowserClass <- R6Class_("envBrowser",
    inherit = HTMLClass,
    public = list(
        envir = NULL,
        name = NULL,
        all.names = FALSE,
        pattern = NULL,
        mode = NULL,
        initialize = function(pos = -1, name, envir, all.names = FALSE, pattern, 
                              mode = "any"){
            super$initialize()
            if (missing(envir)) 
                envir <- as.environment(pos)
            table <- env_browser_table(pos=pos,
                                       name=name,
                                       envir=envir,
                                       all.names=all.names,
                                       pattern=pattern,
                                       mode=mode)
            text_html <- paste(
                table,
                sep="\n")
            self$value <- text_html
            self$name <- name
            self$envir <- envir
            self$all.names <- all.names
            self$pattern <- pattern
            self$mode <- mode
        },
        refresh = function(){
            table <- env_browser_table(name=self$name,
                                       envir=self$envir,
                                       all.names=self$all.names,
                                       pattern=self$pattern,
                                       mode=self$mode)
            text_html <- paste(
                table,
                sep="\n")
            self$value <- text_html
        }
    )                   
)

#' @export
envBrowser <- function(...) envBrowserClass$new(...)
envBrowserClass <- R6Class_("envBrowser",
    inherit = HTMLClass,
    public = list(
        envir = NULL,
        name = NULL,
        all.names = FALSE,
        pattern = NULL,
        mode = NULL,
        initialize = function(name = NULL,
                              envir,
                              all.names = FALSE,
                              pattern = NULL, 
                              mode = "any"){
            super$initialize()
            table <- env_browser_table(name=name,
                                       envir=envir,
                                       all.names=all.names,
                                       if(length(pattern)) pattern=pattern,
                                       mode=mode,
                                       include_css=TRUE)
            text_html <- paste(
                table,
                sep="\n")
            self$value <- text_html
            self$name <- name
            self$envir <- envir
            self$all.names <- all.names
            self$pattern <- pattern
            self$mode <- mode
            eventmanagers$eval$on("cell_completed",self$refresh)
        },
        refresh = function(){
            table <- env_browser_table(name=self$name,
                                       envir=self$envir,
                                       all.names=self$all.names,
                                       if(length(self$pattern)) pattern=self$pattern,
                                       mode=self$mode,
                                       include_css=TRUE)
            text_html <- paste(
                table,
                sep="\n")
            self$value <- text_html
        }
    )                   
)

#' @export
envBrowser <- function(pos = -1,
                       name = NULL,
                       envir,
                       all.names = FALSE,
                       pattern = NULL, 
                       mode = "any") {
    if (missing(envir)) 
        envir <- as.environment(pos)
    envBrowserClass$new(name=name,
                        envir=envir,
                        all.names=all.names,
                        if(!missing(pattern))pattern=pattern,
                        mode=mode)
}
