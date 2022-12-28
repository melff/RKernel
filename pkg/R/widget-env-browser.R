#' @include widget.R env-browser.R

envBrowserClass <- R6Class_("envBrowser",
    inherit = HTMLClass,
    public = list(
        envir = NULL,
        parent = NULL,
        name = NULL,
        all.names = FALSE,
        pattern = NULL,
        mode = NULL,
        initialize = function(pos = -1,
                              name,
                              envir,
                              parent = NULL,
                              all.names = FALSE,
                              pattern = NULL,
                              mode = "any"){
            super$initialize()
            if (missing(envir)) 
                envir <- as.environment(pos)
            table <- env_browser_table(pos=pos,
                                       name=name,
                                       envir=envir,
                                       parent=parent,
                                       all.names=all.names,
                                       pattern=pattern,
                                       mode=mode,
                                       include_css=TRUE)
            text_html <- paste(
                table,
                sep="\n")
            self$value <- text_html
            self$name <- name
            self$envir <- envir
            self$parent <- parent
            self$all.names <- all.names
            self$pattern <- pattern
            self$mode <- mode
        },
        refresh = function(){
            table <- env_browser_table(name=self$name,
                                       envir=self$envir,
                                       parent=self$parent,
                                       all.names=self$all.names,
                                       pattern=self$pattern,
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
                       parent = NULL,
                       all.names = FALSE,
                       pattern = NULL, 
                       mode = "any") {
    if (missing(envir)) 
        envir <- as.environment(pos)
    envBrowserClass$new(name=name,
                        envir=envir,
                        parent=parent,
                        all.names=all.names,
                        if(!missing(pattern))pattern=pattern,
                        mode=mode)
}
register_export(envBrowser)
