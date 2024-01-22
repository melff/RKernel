#' @description
#' Provide completion for code given at point.
#' @param code A character string with code to be checked for
#'    completions.
#' @param cursor_pos An integer, the current position of the cursor.
get_completions <- function(code,cursor_pos){
    if(!private$completions_inited) private$init_completions()

    lines <- splitLines(code)
    line_length <- nchar(lines)
    line_start <- head(c(0,cumsum(line_length + 1)),-1)
    line_end <- line_start + line_length
    suppressWarnings(i <- which(line_start <= cursor_pos & cursor_pos <= line_end))
    if(!length(i) || !is.finite(i)) return(NULL)
    pos <- cursor_pos - line_start[i] 
    line <- lines[i]
    private$cf$assignLinebuffer(line)
    private$cf$assignEnd(pos)
    match_info <- private$cf$guessTokenFromLine(update=FALSE)
    private$cf$guessTokenFromLine()
    private$cf$completeToken()

    matches <- private$cf$retrieveCompletions()
    start <- line_start[i] + match_info$start
    end <- start + nchar(match_info$token)

    return(list(
        matches = as.list(matches),
        start = start,
        end = end
    ))
}

init_completions = function(){
    utils_ns <- asNamespace('utils')
    private$cf$assignLinebuffer <- get(".assignLinebuffer",utils_ns)
    private$cf$assignEnd <- get(".assignEnd",utils_ns)
    private$cf$guessTokenFromLine <- get(".guessTokenFromLine",utils_ns)
    private$cf$completeToken <- get(".completeToken",utils_ns)
    private$cf$retrieveCompletions <- get(".retrieveCompletions",utils_ns)
    private$completions_inited <- TRUE
}
