HelpServer <- R6Class("HelpServer",
    public = list(
        initialize = function(port = getOption("help.ports")[0]){
            private$http_port <- port # Provided by the frontend
        },
        get_url = function(){
            if(!length(private$http_url))
                self$start()
            return(private$http_url)
        },
        get_port = function(){
            return(private$http_port)
        },
        start = function(){
            options(help.ports = private$http_port)
            suppressMessages(port <- tools::startDynamicHelp(TRUE))
            if(private$http_port!=port){
                warning(sprintf("Changed help port from %d to %d",
                                private$http_port,
                                port))
                options(http.port = port)
                private$http_port <- port
            }
            if(private$use_proxy){
                http_url <- sprintf("/proxy/%d",port)
                if(nzchar(Sys.getenv("JUPYTERHUB_SERVICE_PREFIX"))){
                    http_url <- paste0(Sys.getenv("JUPYTERHUB_SERVICE_PREFIX"),
                                       http_url)
                    http_url <- gsub("//","/",http_url,fixed=TRUE)
                }
            }
            else {
                http_url <- sprintf("http://localhost:%d",port)
            }
            private$http_url <- http_url
        }
    ),
    private = list(
        http_url = NULL,
        http_port = 0,
        use_proxy = FALSE
    )
)

help_server <- new.env()

#' @export
set_help_port <- function(port){
    help_server$current <- HelpServer$new(port)
    options(help.ports=port)
    replace_in_package("tools","example2html",example_html)
}

get_help_url <- function(){
    url <- help_server$current$get_url()
    return(url)
}

#' @export
set_help_displayed <- function(on=TRUE){
    if(on){
        add_displayed_classes("help_files_with_topic")
        add_displayed_classes("hsearch")
    }
    else {
        remove_displayed_classes("help_files_with_topic")
        remove_displayed_classes("hsearch")
    }
}

popout_button_style <- '<style>
form.help-popout-button button {
     border-style:solid;
     border-width:1px;
     border-radius:0;
     border-color:#cfcfcf;
}
#pager-container form.help-popout-button {
     display:none;
}
iframe.manpage {
   border-style:solid;
   border-width:1px;
   border-color:#cfcfcf;
   border-radius:2px;
}
#pager-container iframe.manpage {
     height: 5000em;
}
</style>
'
popout_button <- '<form class="help-popout-button" action="%s" method="get" target="_blank">
             <button type="submit">Open in new tab</button>
</form>'


#' @describeIn display_data S3 method for help pages
#' @importFrom uuid UUIDgenerate
#' @importFrom utils URLencode
#' @importFrom tools Rd2HTML Rd2txt Rd2latex
#' @export 
display_data.help_files_with_topic <- function(x,...,
                                          id=UUIDgenerate(),
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
        
        help_page_height <- getOption("help_page_height","60ex")

        for(file in paths){
            pkgname <- basename(dirname(dirname(file)))
            Rd <- getHelpFile(file)
            text_plain1 <- capture.output(Rd2txt(Rd, package = pkgname, outputEncoding = 'UTF-8'))
            # text_latex1 <- capture.output(Rd2latex(Rd, package = pkgname, outputEncoding = 'UTF-8'))
            text_plain <- c(text_plain,text_plain1)
            # text_latex <- c(text_latex,text_latex1)
            help_label1 <- paste0(pkgname,"::",topic)
            help_labels <- c(help_labels,help_label1)
            help_url1 <- paste0(get_help_url(),"/library/",pkgname,"/html/",basename(file),".html")
            help_urls <- c(help_urls,help_url1)
        }

        if(length(paths) == 1){
            help_url <- paste0(get_help_url(),"/library/",pkgname,"/html/",basename(paths),".html")
            text_html <- paste(paste0(
                "<iframe src='",help_url,"'"),
                "style='width:100%;height:",help_page_height,";'",
                "class='manpage'>",
                # "frameborder='0' onload=window.parent.scrollTo(0,0)>",
                "</iframe>",
                sep="\n")
        } 
        else {
            help_url <- paste0(get_help_url(),"/library/NULL/help/",URLencode(topic,reserved=TRUE))
            text_html <- paste(paste0("<iframe src='",help_url,"'"),
                "style='width:100%;height:",help_page_height,"'",
                "class='manpage'>",
                # "frameborder='0' onload=window.parent.scrollTo(0,0)>",
                "</iframe>",
                sep="\n")
        }

        popout_button <- sprintf(popout_button,help_url)
        text_html <- paste(text_html,popout_button_style,popout_button,sep="\n")

    } 
    else {
        text_plain <- paste(gettextf('No documentation for %s in specified packages and libraries:', sQuote(topic)),
                            gettextf('you could try %s', sQuote(paste0('??', topic))),
                            sep = '\n')
        text_html <- text_plain
        # text_latex <- text_plain
    }

    mime_data <- list(
        "text/plain"=paste(text_plain,collapse="\n"),
        "text/html"=paste(text_html,collapse="\n"),
        "text/latex"="" # paste(text_latex,collapse="\n")
    )


    d <- list(data=mime_data)
    d$metadata <- emptyNamedList
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}


#' Start interactive help system
#'
#' @description A variant of \code{\link[utils]{help.start}} that works when called from inside a
#'     Jupyter notebook.
#'
#' @param update A logical value. This formal argument exists for compatibility
#'     reasons only.
#' @param gui A character string. This formal argument exists for compatibility
#'     reasons only.
#' @param browser A character string. This formal argument exists for compatibility
#'     reasons only.
#' @param remote A character string. This formal argument exists for compatibility
#'     reasons only.
#' @export
help.start <- function(update = FALSE, 
                       gui = "irrelevant", 
                       browser = getOption("browser"), 
                       remote = NULL){
    help_url <- paste0(get_help_url(),"/doc/html/index.html")
    text_html <- paste(paste0("<iframe src='",help_url,"'"),
        "style='width:100%;height:70ex;'",
        "class='manpage'>",
        # "onload=window.parent.scrollTo(0,0)>",
        "</iframe>",
        sep="\n")
    popout_button <- sprintf(popout_button,help_url)
    text_html <- paste(text_html,popout_button_style,popout_button,sep="\n")
    mime_data <- list(
        "text/plain"=character(0),
        "text/html"=paste(text_html,collapse="\n"),
        "text/latex"=""
    )

    d <- list(data=mime_data)
    d$metadata <- emptyNamedList
    d$transient <- list(display_id=UUIDgenerate())
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

help_start <- help.start





#' @describeIn display_data S3 method for results of 'help.search()'
#' @export
display_data.hsearch <- function(x,..., 
    id=UUIDgenerate(), 
    update=FALSE){
    
    matches <- x$matches
    matches <- unique(matches[c("Topic","Title","Package","Type")])
    matches$Name <- paste0(matches$Package,"::",matches$Topic)
    
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
                        "style='width: 100%;'",
                        "class='manpage'",
                        "frameborder='0' seamless onload=window.parent.scrollTo(0,0)>",
                        "</iframe>",
                        sep="\n")
    style <- '<style>
    form.help-popout-button {
         border-style:revert;
    }
    #pager-container form.help-popout-button {
         display:none;
    }
    iframe.manpage {
         height: 30rem;
    }
    #pager-container iframe.manpage {
         height: 5000em;
    }
    </style>
    '
    popout_button <- '<form class="help-popout-button" action="%s" method="get" target="_blank">
         <button type="submit">Open in new tab</button>
    </form>'
    popout_button <- sprintf(popout_button,help_search_url)
    text_html <- paste(style,text_html,popout_button,sep="\n")

    mime_data <- list("text/plain"=paste(text_plain,collapse="\n"),
                      "text/html"=paste(text_html,collapse="\n"))
    d <- list(data=mime_data)
    d$metadata <- emptyNamedList
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

my_example2html <- function(topic, package, Rhome = "", env = NULL){
    "Not yet implemented"
}

example_html <- function(topic,package = NULL,...) {
    # topic <- deparse(substitute(topic))
    g <- graphics$current
    g$delivery_mode <- "svg"
    on.exit(g$delivery_mode <- "display")
    e <- capture.output(example(topic,package,local=TRUE,
                                character.only=TRUE))
    
    e <- paste(e,collapse="\n")
    e <- strsplit(e,"\x10",fixed=TRUE)[[1]]
    
    r <- character()
    n <- nchar("<!DOCTYPE svg>")
    for(i in seq_along(e)){
        if(startsWith(e[i],"<!DOCTYPE svg>")){
            r[i] <- paste0("<div class= 'figure'>",substr(e[i],start=n+1,nchar(e[i])),"</div>")
        } else {
            r[i] <- paste0("<pre>",e[i],"\n</pre>")
        }
    }
    head <- c("<!DOCTYPE html>","<html>")
    style <- "<style>
  .doc {
    width: 90ex;
    margin: 0 auto;
  }
  .figure {
    margin: 0 auto;
    background: 'white';
  }
</style>"
    description  <- paste("Examples for",package,"::",topic)
    title <- paste0("<title>",description,"</title>")
    head <- c(head,"<head>")
    head <- c(head,title,style)
    head <- c(head,"</head>")
    headline <- c("<h1>",description,"</h1>")
    body <- c(headline,r)
    help_url <- paste0(get_help_url(),"/library/",package,"/html/",topic,".html")
    back <- c("<p>",sprintf("<a href=\"%s\">",help_url),"Back to help page","</a>","</p>")
    body <- c(body,back)
    body <- c("<body>","<div class=\"doc\">",body,"</div>","</body>")
    tail <- "</html>"

    r <- paste(c(head,body,tail),collapse="\n")
    return(list(payload=r))
}

demo_html <- function(topic,package = NULL,...) {
    topic <- deparse(substitute(topic))
    g <- graphics$current
    g$delivery_mode <- "svg"
    on.exit(g$delivery_mode <- "display")
    e <- capture.output(demo(topic=topic,package,ask=FALSE,character.only=TRUE))
    e <- c(e,capture.output(send_changed_graphics()))
    e <- paste(e,collapse="\n")
    e <- strsplit(e,"\x10",fixed=TRUE)[[1]]

    r <- character()
    n <- nchar("<!DOCTYPE svg>")
    for(i in seq_along(e)){
        if(startsWith(e[i],"<!DOCTYPE svg>")){
            r[i] <- paste0("<div class= 'figure'>",substr(e[i],start=n+1,nchar(e[i])),"</div>")
        } else {
            r[i] <- paste0("<pre>",e[i],"\n</pre>")
        }
    }
    r <- paste0(r,collapse="\n")
    head <- "<!DOCTYPE html>\n<html>\n"
    style<- "<style>
  .doc {
    width: 90ex;
    margin: 0 auto;
  }
  .figure {
    margin: 0 auto;
    background: 'white';
  }
</style>"
    body <- paste0("<body>\n<div class=\"doc\">\n",r,"</div>\n</body>")
    tail <- "</html>"

    r <- paste(head,style,body,tail,collapse="\n")
    return(list(payload=r))
}
