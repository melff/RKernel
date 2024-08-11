cell_saved <- new.env()


#' Set options locally for the current jupyter notebook cell
#' @param ... Options, see \code{\link{options}}.
#' @export
cell.options <- function(...){
    # log_out("cell.options")
    args <- list(...)
    nms <- names(args)
    op <- lapply(nms,getOption)
    names(op) <- nms
    # log_out(args,use.print=TRUE)
    # log_out(op,use.print=TRUE)
    cell_saved$options <- op
    do.call("options",args)
}

#' Set graphics parameters locally for the current jupyter notebook cell
#' @param ... Graphics parameters, see \code{\link{par}}.
#' @export
cell.par <- function(...){
    # log_out("cell.par")
    args <- list(...)
    nms <- names(args)
    op <- lapply(nms,par)
    names(op) <- nms
    cell_saved$pars <- op
    do.call("par",args)
}

restore_saved_opts <- function(){
    # log_out("restore_saved_opts")
    op <- cell_saved$options
    cell_saved$options <- NULL
    # log_out(names(op),use.print=TRUE)
    if(length(op) > 0)
        do.call("options",op)
}

restore_saved_pars <- function(){
    # log_out("restore_saved_pars")
    op <- cell_saved$pars
    cell_saved$pars <- NULL
    # log_out(names(op),use.print=TRUE)
    if(length(op) > 0)
        do.call("par",op)
}

#'@export
install_cell_hooks <- function(){
    setHook('cell-end',restore_saved_opts)
    setHook('cell-end',restore_saved_pars)
}
