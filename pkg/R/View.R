#' Invoke a Data Viewer
#'
#' This is a re-implementation of \code{\link[utils]{View}} that works within
#' Jupyter notebooks by leveraging the \href{https://ipywidgets.readthedocs.io}{Jupyter widgets} infrastructure
#' or by using the DataTable Javascript library. The latter is the case if the system option "View.backend"
#' is set to "dataTable" or if this option is not set. Otherwise a 'virtable_widget' is used.
#'
#' @param x An \R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @param title A string used as title. Currently unused.
#' 
#' @export
View <- function(x,
                 title=deparse(substitute(x)),
                 ...) UseMethod("View")

#' @include evaluator.R
register_export(View)

#' @rdname View
#' @param ... Other arguments, ignored.
#' @export
View.default <- function(x,title=deparse(substitute(x)),...)
{

    # cls <- class(x)
    # title <- paste0(cls,": ",title)
    title <- paste0(title,collapse="")
    x <- as.data.frame(x)
    if(ncol(x) == 1)
        colnames(x) <- title
    if(getOption("View.backend","virtable_widget")=="dataTable")
        dataTable(x)
    else
        virtable_widget(x)
    
}

#' @rdname View
#' @param ... Other arguments, ignored.
#' @export
View.data.frame <- function(x,title=deparse(substitute(x)),...)
{

    # cls <- class(x)
    # title <- paste0(cls,": ",title)
    if(getOption("View.backend","virtable_widget")=="dataTable")
        dataTable(x)
    else
        virtable_widget(x)
}

