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
    forcedims <- getOption("jupyter.htmlwidget.force.dims",TRUE)
    padding <- getOption("jupyter.htmlwidget.padding",20L)
    mime_data <- list()
    for(mime_type in rkernel_mime_types){
        repr_func <- mime2repr[[mime_type]]
        repr_result <- repr_func(x,...)
        mime_data[[mime_type]] <- repr_result
    }
    # This is needed to make htmlwidgets appear in Firefox
    if("text/html" %in% names(mime_data)){
        r_html <- mime_data["text/html"]
        height_match <- getMatch(r_html,regexec("height:([0-9]+)(.*?);",r_html))
        width_match <- getMatch(r_html,regexec("width:([0-9]+)(.*?);",r_html))
        if(length(height_match) > 1){
            if(forcedims) {
                height <- getOption("jupyter.plot.height",6)
                height_units <- getOption("jupyter.plot.units","in")
                height_res <- getOption("jupyter.plot.res",96)
                if(height_units %in% c("in","cm","mm")){
                    height <- height*height_res
                    if(height_units == "cm")
                        height <- round(height/2.54,0)
                    else if(height_units == "mm")
                        height <- round(height/25.4,0)
                }
                height_units <- "px"
                style_height <- sprintf("height:%d%s;",height,height_units)
                r_html <- sub("height:([0-9]+)(.*?);",style_height,r_html)
            }
            else {
                height <- as.integer(height_match[2])
                height_units <- height_match[3]
            }
            if(height_units == "%")
                height <- paste0(height,height_units)
            else
                height <- height + padding
        }
        else
            height <- 500
        if(length(width_match) > 1){
            if(forcedims){
                width <- getOption("jupyter.plot.width",6)
                width_units <- getOption("jupyter.plot.units","in")
                width_res <- getOption("jupyter.plot.res",96)
                if(width_units %in% c("in","cm","mm")){
                    width <- width*width_res
                    if(width_units == "cm")
                        width <- round(width/2.54,0)
                    else if(width_units == "mm")
                        width <- round(width/25.4,0)
                }
                width_units <- "px"
                style_width <- sprintf("width:%d%s;",width,width_units)
                r_html <- sub("width:([0-9]+)(.*?);",style_width,r_html)
            }
            else {
                width <- as.integer(width_match[2])
                width_units <- width_match[3]
            }
            if(width_units == "%")
                width <- paste0(width,width_units)
            else
                width <- width + padding
        }
        else
            width <- 500
        r_html <- gsub("\n","",r_html,fixed=TRUE)
        r_html <- gsub("\t","",r_html,fixed=TRUE)
        r_html <- htmlEscape(r_html)
        r_html <- sprintf("<div>\n<iframe srcdoc='%s' width='%s' height='%s' frameborder='0'>\n</iframe>\n</div>\n",
                         r_html,width,height)
        mime_data["text/html"] <- r_html
    }
    d <- list(data=mime_data)
    d$metadata <- metadata
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

getMatch <- function(x,match){
    if(length(match) < 0) return(character(0))
    if(is.list(match))
        match <- match[[1]]
    lens <- attr(match,"match.length")
    n <- length(match)
    res <- character(n)
    for(i in 1:n){
        start <- match[i]
        end <- start + lens[i] - 1
        res[i] <- substr(x,start=start,stop=end)
    }
    res
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

#' @importFrom tools Rd2HTML Rd2txt Rd2latex
display.help_files_with_topic <- function(x,...,
                                          id=uuid::UUIDgenerate(),
                                          update=FALSE){

    paths <- as.character(x)
    topic <- attr(x,"topic")

    if (length(paths) == 0) {
        return(paste(gettextf('No documentation for %s in specified packages and libraries:', sQuote(topic)),
                     gettextf('you could try %s', sQuote(paste0('??', topic))), sep = '\n'))
    } else if(length(paths) > 1) {
        message(sprintf("More than one help page on topic %s, using the first",sQuote(topic)))
    }

    file <- paths[[1]]
    pkgname <- basename(dirname(dirname(file)))

    utils_ns <- asNamespace("utils")
    getHelpFile <- get(".getHelpFile",utils_ns)
    Rd <- getHelpFile(file)

    text_plain <- capture.output(Rd2txt(Rd, package = pkgname, outputEncoding = 'UTF-8'))
    text_html <- capture.output(Rd2HTML(Rd, package = pkgname, outputEncoding = 'UTF-8'))
    text_latex <- capture.output(Rd2latex(Rd, package = pkgname, outputEncoding = 'UTF-8'))

    head_end_idx <- grep("</head><body>",text_html)
    body_end_idx <- grep("</body></html>",text_html)
    lines_to_remove <- c(seq_len(head_end_idx),body_end_idx)
    text_html <- text_html[-lines_to_remove]

    mime_data <- list(
        "text/plain"=paste(text_plain,collapse="\n"),
        "text/html"=paste(text_html,collapse="\n"),
        "text/latex"=paste(text_latex,collapse="\n")
    )

    d <- list(data=mime_data)
    d$metadata <- namedList()
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

javascript <- function(text,file){
    if(missing(text)){
        text <- readLines(file)
    }
    text <- paste(text,collapse="\n")
    text_plain <- paste("Javascript: ",text,sep="\n")
    text_html <- paste("<script>",text,"</script>",sep="\n")
    display("text/plain"=text_plain,
            "text/html"=text_html)
}

LaTeXMath <- function(text){
    text <- paste(text,collapse="\n")
    text_plain <- paste("Math:",text,sep="\n")
    text_latex <- paste0("$$",text,"$$")
    display("text/plain"=text_plain,
            "text/latex"=text_latex)
}
