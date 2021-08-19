#' @importFrom repr mime2repr

#' @export
display <- function(x,...) UseMethod("display")
#' @export
display.default <- function(x,...,
                            metadata=NULL,
                            id=uuid::UUIDgenerate(),
                            update=FALSE){

    if(missing(x)){
        if(!length(metadata)) metadata <- namedList()
        d <- list(data=list(...))
    } 
    else {
        rkernel_mime_types <- getOption("rkernel_mime_types",
                                        c("text/plain",
                                          "text/html",
                                          "text/latex",
                                          "text/markdown"))
        mime_data <- list()
        for(mime_type in rkernel_mime_types){
            repr_func <- mime2repr[[mime_type]]
            repr_result <- repr_func(x,...)
            mime_data[[mime_type]] <- repr_result
        }
        d <- list(data=mime_data)
    }
    d$metadata <- metadata
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

#' @importFrom htmltools htmlEscape
#' @export
display.htmlwidget <- function(x,...,
                            metadata=NULL,
                            id=uuid::UUIDgenerate(),
                            update=FALSE){
    rkernel_mime_types <- getOption("rkernel_mime_types",
                                    c("text/plain",
                                      "text/html"))
    rkernel_mime_types <- intersect(rkernel_mime_types,
                                   c("text/plain",
                                     "text/html"))
    mime_data <- list()
    for(mime_type in rkernel_mime_types){
        repr_func <- mime2repr[[mime_type]]
        repr_result <- repr_func(x,...)
        mime_data[[mime_type]] <- repr_result
    }
    # This is needed to make htmlwidgets appear in Firefox
    if("text/html" %in% names(mime_data)){
        r_html <- mime_data["text/html"]
        r_html <- gsub("\n","",r_html,fixed=TRUE)
        r_html <- gsub("\t","",r_html,fixed=TRUE)
        r_html <- htmlEscape(r_html)
        r_html <- paste0("<div>\n<iframe srcdoc='",
                         r_html,
                         "' width='100%' height='500' frameborder='0'>\n</iframe>\n</div>\n")
        # TODO obtain height from the widget object itself
        mime_data["text/html"] <- r_html
    }
    d <- list(data=mime_data)
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
Page <- function(x,...) UseMethod("Page")
#' @export
Page.default <- function(x,start=1,...){
    if(missing(x)){
        data <- list(...)
    } 
    else {
        displayed <- display(x=x,...)
        data <- displayed$data
    }
    p <- list(source="page",
              data=data,
              start=start)
    structure(p,class="payload")
}

#' @export
add_displayed_classes <- function(x){
    if(is.character(x)) classes <- x
    else classes <- class(x)
    if(length(classes)){
        displayed_classes <- getOption("rkernel_displayed_classes")
        options(rkernel_displayed_classes=union(displayed_classes,classes))
    }
}
#' @export
remove_displayed_classes <- function(x){
    if(is.character(x)) classes <- x
    else classes <- class(x)
    if(length(classes)){
        displayed_classes <- getOption("rkernel_displayed_classes")
        options(rkernel_displayed_classes=setdiff(displayed_classes,classes))
    }
}

#' @export
add_paged_classes <- function(x){
    if(is.character(x)) classes <- x
    else classes <- class(x)
    if(length(classes)){
        paged_classes <- getOption("rkernel_paged_classes")
        options(rkernel_paged_classes=union(paged_classes,classes))
    }
}
#' @export
remove_paged_classes <- function(x){
    if(is.character(x)) classes <- x
    else classes <- class(x)
    if(length(classes)){
        paged_classes <- getOption("rkernel_paged_classes")
        options(rkernel_paged_classes=setdiff(paged_classes,classes))
    }
}
