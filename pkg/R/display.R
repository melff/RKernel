#' @export
display <- function(x,...) UseMethod("display")
#' @export
display.default <- function(x,...,metadata=NULL,id=uuid::UUIDgenerate(),update=FALSE){
   if(!length(metadata)) metadata <- namedList()
   d <- list(data=list(...))
   d$metadata <- metadata
   d$transient <- list(display_id=id)
   if(update) cl <- "update_display_data"
   else cl <- "display_data"
   structure(d,class=cl)
}
#' @export
display_id <- function(x) UseMethod("display_id")
#' @export
display_id.display_data <- function(x) x$transient$display_id
#' @export
display_id.update_display_data <- function(x) x$transient$display_id

#' @export
update.display_data <- function(object,...){
    id <- display_id(object)
    display(...,id=id,update=TRUE)
}


#' @export
jkpage <- function(x,...) UseMethod("jkpage")
#' @export
jkpage.default <- function(x,start=1,...){
   p <- list(source="page",data=list(...),start=start)
   structure(p,class="payload")
}
