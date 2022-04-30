#' Main entry point of the package
#'
#' @description This function reads the connection info file, creases a "Kernel"
#'     object and runs it, i.e. starts the kernel.
#' 
#' @export
main <- function(){
    connection_file <- commandArgs(TRUE)[[1]]
    connection_info <- fromJSON(connection_file)
    kernel <- Kernel$new(connection_info)
    kernel$run()
    #kernel$finalize()
}
