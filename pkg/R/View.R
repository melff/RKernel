#' @export
View <- function(x,
                 title=deparse(substitute(x)),
                 ...)
{

    # cls <- class(x)
    # title <- paste0(cls,": ",title)

    virtable_widget(x)
    
}

