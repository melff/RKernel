#' @export
main <- function(){
    connection_file <- commandArgs(TRUE)[[1]]
    connection_info <- fromJSON(connection_file)
    evaluator <- Evaluator$new()
    comm_dispatcher <- CommDispatcher$new()
    kernel <- Kernel$new(connection_info,
                         evaluator=evaluator,
                         comm_dispatcher=comm_dispatcher)
    kernel$run()
    #kernel$finalize()
}
