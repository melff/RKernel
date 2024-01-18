replace_in_package <- function(pkg_name,name,value){
  env_name <- paste0("package:",pkg_name)
  if(env_name %in% search())
    env <- as.environment(env_name)
  else
    env <- getNamespace(pkg_name)
  .BaseNamespaceEnv$unlockBinding(name, env)
  assign(name, value, env)
  .BaseNamespaceEnv$lockBinding(name, env)
}

sync_nr <- 0L

sync_request <- function(...){
    sync_nr <<- sync_nr + 1L
}

synced_cat <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
    append = FALSE) 
{
    if (is.character(file)) 
        if (file == "") 
            file <- stdout()
        else if (startsWith(file, "|")) {
            file <- pipe(substring(file, 2L), "w")
            on.exit(close(file))
        }
        else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    .Internal(cat(list(...), file, sep, fill, labels, append))
    sync_request()
}


synced_print.default <- function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, 
    right = FALSE, max = NULL, width = NULL, useSource = TRUE, 
    ...) 
{
    args <- pairlist(digits = digits, quote = quote, na.print = na.print, 
        print.gap = print.gap, right = right, max = max, width = width, 
        useSource = useSource, ...)
    missings <- c(missing(digits), missing(quote), missing(na.print), 
        missing(print.gap), missing(right), missing(max), missing(width), 
        missing(useSource))
    .Internal(print.default(x, args, missings))
    sync_request()
}

replace_in_package("base","cat",synced_cat)

replace_in_package("base","print.default",synced_print.default)
