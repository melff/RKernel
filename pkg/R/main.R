#' @export
main <- function(){
    connection_file <- commandArgs(TRUE)[[1]]
    connection_info <- fromJSON(connection_file)
    kernel <- Kernel$new(connection_info)
    kernel$run()
    #kernel$finalize()
}
