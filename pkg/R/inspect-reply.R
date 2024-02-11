inspect_reply <- function(code,cursor_pos,detail_level=0){
    # log_out("inspect_reply")
    word <- find_word_at(code,cursor_pos)
    # log_out(word,use.str=TRUE)

    found <- FALSE
    if(!nzchar(word)){
        d <- emptyNamedList
    }
    else if(detail_level == 0){

        obj <- get0(word,envir=.GlobalEnv)
        if(!is.null(obj)){
            result_text <- capture.output(str_(obj))
            result_text <- paste(result_text,collapse="\n")
            result_text <- paste(word,result_text,sep=": ")
            d <- list(
                `text/plain` = result_text
            )
            found <- TRUE
        }
        else {
            d <- emptyNamedList
            found <- FALSE
        }
    }
    else {

        if(startsWith(word,"??")){
            word <- "help.search"
        } else if(startsWith(word,"?")){
            word <- "help"
        }

        if(word %in% ls(pos=1L))
            h <- NULL
        else
            h <- help(word)

        if(length(h) > 0){
            d <- display_data(h,embedded=TRUE,
                              include_button=FALSE)
            d <- d$data
            found <- TRUE
        } 
        else 
        {
            FUN <- get0(word,envir=.GlobalEnv, mode = "function")

            if(length(FUN) > 0){
                result_text <- paste0(word,": ")
                result_text <- c(result_text,
                                 capture.output(print_(FUN)))
                result_text <- paste(result_text,collapse="\n")
                found <- TRUE
            }
            else {
                obj <- get0(word,envir=.GlobalEnv)
                if(length(obj)){
                    result_text <- paste0(word,": ")

                    result_text <- c(result_text,
                                     capture.output(str_(obj)))
                    result_text <- paste(result_text,collapse="\n")
                    found <- TRUE
                }
                else {
                    result_text <- paste0(word,": [not found]")
                    found <- FALSE
                }
            }
            d <- list(
                `text/plain` = result_text
            )
        }

    }

    list(
        found = found,
        data = d
    )
}

find_word_left <- function(x){
    m <- regexpr("[?]*[A-Za-z.][A-Za-z0-9._]*$",x)
    regmatches(x,m)
}

find_word_right <- function(x){
    m <- regexpr("^[A-Za-z0-9._]*",x)
    regmatches(x,m)
}


split_at <- function(x,pos){
    if(pos == 0)
        left <- ""
    else
        left <- substr(x,
                       start = 1,
                       stop  = pos)
    right <- substr(x,
                    start = pos + 1L,
                    stop  = nchar(x))
    list(left  = left,
          right = right)
}

find_word_at <- function(x,pos){
    l <- split_at(x,pos)
    left <- find_word_left(l$left)
    right <- find_word_right(l$right)
    paste0(left,right)
}
