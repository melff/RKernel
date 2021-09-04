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

    utils_ns <- asNamespace("utils")
    getHelpFile <- get(".getHelpFile",utils_ns)

    if(length(paths)>=1){
        text_plain <- character(0)
        text_latex <- character(0)
        help_urls <- character(0)
        help_labels <- character(0)

        for(file in paths){
            pkgname <- basename(dirname(dirname(file)))
            Rd <- getHelpFile(file)
            text_plain1 <- capture.output(Rd2txt(Rd, package = pkgname, outputEncoding = 'UTF-8'))
            text_latex1 <- capture.output(Rd2latex(Rd, package = pkgname, outputEncoding = 'UTF-8'))
            text_plain <- c(text_plain,text_plain1)
            text_latex <- c(text_latex,text_latex1)
            help_label1 <- paste0(pkgname,"::",topic)
            help_labels <- c(help_labels,help_label1)
            help_url1 <- paste0(get_help_url(),"/library/",pkgname,"/html/",basename(file),".html")
            help_urls <- c(help_urls,help_url1)
        }
    } 
    else {
        text_plain <- paste(gettextf('No documentation for %s in specified packages and libraries:', sQuote(topic)),
                      gettextf('you could try %s', sQuote(paste0('??', topic))), sep = '\n')
        text_latex <- text_plain
    }

    if(length(paths) == 1){
        help_url <- paste0(get_help_url(),"/library/",pkgname,"/html/",basename(paths),".html")
        text_html <- paste(paste0("<iframe src='",help_url,"'"),
                           "style='width: 100%; height: 5000em;'",
                           "id='manpage'",
                           "frameborder='0' seamless>",
                           "</iframe>",
                           sep="\n")
    } 
    else {
        help_url <- paste0(get_help_url(),"/library/NULL/help/",URLencode(topic,reserved=TRUE))
        text_html <- paste(paste0("<iframe src='",help_url,"'"),
                           "style='width: 100%; height: 5000em;'",
                           "id='manpage'",
                           "frameborder='0' seamless>",
                           "</iframe>",
                           sep="\n")
    }


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

#' @export
display.hsearch <- function(x,..., 
    id=uuid::UUIDgenerate(), 
    update=FALSE){
    
    matches <- x$matches
    matches <- unique(matches[c("Topic","Title","Package","Type")])
    matches <- within(matches,
                      Name <- paste0(Package,"::",Topic))
    
    matches <- split(matches,matches$Type)
    text_out_grp <- lapply(matches,tformat_mgroup)
    text_plain <- character(0)
    if("help" %in% names(text_out_grp)){
        text_plain <- c(text_plain,"Help pages:","",text_out_grp$help,"")
    }
    if("vignette" %in% names(text_out_grp)){
        text_plain <- c(text_plain,"Vignettes:","",text_out_grp$vignette,"")
    }
    if("demo" %in% names(text_out_grp)){
        text_plain <- c(text_plain,"Demos:","",text_out_grp$demo,"")
    }

# http://127.0.0.1:10006/doc/html/Search?pattern=regression&
#fields.alias=1&fields.title=1&fields.concept=1&ignore.case=1&types.help=1&types.vignette=1&types.demo=1

    help_search_url <- paste0(get_help_url(),
                  "/doc/html/Search?pattern=",
                  gsub(" ","+",x$pattern,fixed=TRUE))
    for(field in x$fields){
        help_search_url <- paste0(help_search_url,paste0("&fields.",field,"=1"))
    }
    if(x$ignore.case)
        help_search_url <- paste0(help_search_url,"&ignore.cases=1")
    if(x$type=="fuzzy")
        help_search_url <- paste0(help_search_url,"&agrep=1")
    else if(is.numeric(x$agrep))
        help_search_url <- paste0(help_search_url,"&agrep=",x$agrep)
    for(type in c("help","vignette","demo")){
        if(type %in% x$types)
            help_search_url <- paste0(help_search_url,paste0("&types.",type,"=1"))
    }
    if(length(x$package)){
        help_search_url <- paste0(help_search_url,paste0("&package",type,"=",x$package))
    }
    text_html <- paste(paste0("<iframe src='",help_search_url,"'"),
                        "style='width: 100%; height: 5000em;'",
                        "id='manpage'",
                        "frameborder='0' seamless onload=window.parent.scrollTo(0,0)>",
                        "</iframe>",
                        sep="\n")

    mime_data <- list("text/plain"=paste(text_plain,collapse="\n"),
                      "text/html"=paste(text_html,collapse="\n"))
    d <- list(data=mime_data)
    #d$metadata <- namedList()
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

tformat_mgroup <- function(matches){
    text_out <- matches[c("Topic","Title")]
    apply(text_out,1,format_item)
}

format_item <- function(x){
    x2 <- x[2]
    x2 <- strwrap(x2,120-7)
    x2 <- paste0("      ",x2)
    paste0(c(x[1],x2,""),collapse="\n")
}


Javascript <- function(text,file){
    if(missing(text)){
        text <- readLines(file)
    }
    text <- paste(text,collapse="\n")
    # text_plain <- paste("Javascript: ",text,sep="\n")
    #text_html <- paste("<script>",text,"</script>",sep="\n")
    display(#"text/plain"="",
            "application/javascript"=text)
}

LaTeXMath <- function(text){
    text <- paste(text,collapse="\n")
    text_plain <- paste("Math:",text,sep="\n")
    text_latex <- paste0("$$",text,"$$")
    display("text/plain"=text_plain,
            "text/latex"=text_latex)
}

raw_html <- function(text,id=uuid::UUIDgenerate()){
    text <- paste(text,collapse="\n")
    display("text/plain"="",
            "text/html"=text,
            id=id)
}


