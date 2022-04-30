#' Invoke a Data Viewer
#'
#' This is a re-implementation of \code{\link[utils]{View}} that works within
#' Jupyter notebooks by leveraging the \href{https://ipywidgets.readthedocs.io}{Jupyter widgets} infrastructure.
#'
#' @param x an \R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @param title a string used as title. Currently unused.
#' 
#' @export
View <- function(x,
                 title=deparse(substitute(x)),
                 ...)
{

    # cls <- class(x)
    # title <- paste0(cls,": ",title)

    virtable_widget(x)
    
}

