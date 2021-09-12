#' @export
View <- function(x,title) UseMethod("View")


# str_ <- function(nm,envir) chartr(r"(\)",r"(\\)",
#           trimws(capture.output(str(get(nm,envir)))))


#' @export
View.default <- function(x,title){
    scrolling_table(x)
}
