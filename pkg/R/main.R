#' @export
main <- function(){
    connection_file <- commandArgs(TRUE)[[1]]
    connection_info <- fromJSON(connection_file)
    evaluator <- Evaluator$new()
    kernel <- Kernel$new(connection_info,
                         evaluator=evaluator)
    kernel$run()
    #kernel$finalize()
}
