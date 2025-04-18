is_unexpected_end <- function(code) 
    grepl(gettext("unexpected end of input",
                  domain = "R"),
          code,fixed = TRUE)

is_unexpected_string <- function(code) 
    grepl(gettextf("unexpected %s","INCOMPLETE_STRING",
                   domain = "R"),
          code,fixed = TRUE)

code_status <- function(code){
    status <- tryCatch({
        str2expression(code)
        "complete"
    },
    error = conditionMessage)
    if(is_unexpected_end(status) || is_unexpected_string(status))
        return("incomplete")
    else if(status!="complete")
        return("invalid")
    else return("complete")
}

code_is_complete <- function(code) code_status(code) == "complete"