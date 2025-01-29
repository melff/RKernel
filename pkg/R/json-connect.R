EOT <- '\x04'
DLE <- '\x10'
ETB <- "\x17"
STX <- "\x02"
ETX <- "\x03"
SO <- "\x0E"
SI <- "\x0F"
APC <- "\x9F"
ST <- "\x9C"


# JSON_BEGIN <- "[!JSON]"
# JSON_END <- "[/!JSON]"

JSON_BEGIN <- SO
JSON_END <- SI

MSG_BEGIN <- JSON_BEGIN
MSG_END <- JSON_END

#' @importFrom utils limitedLabels
json_send <- function(msg, file=stdout()){
    if(!is.list(msg)) {
        log_out("Non-list argument to 'json_send()'")
        calls <- sys.calls()
        calls <- limitedLabels(calls)
        log_print(calls)
    }
    # log_out(msg,use.print=TRUE)
    msg <- to_json(msg)
    cat(DLE,JSON_BEGIN,msg,JSON_END,DLE,sep="",file=file)
}

json_unwrap <- function(x) {
    fromJSON(x,
        simplifyVector = FALSE,
        simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE
    )
}
msg_unwrap <- json_unwrap  

msg_extract <- function(msg) {
    msg <- remove_prefix(msg, DLE) |> remove_suffix(DLE)
    remove_prefix(msg, MSG_BEGIN) |> remove_suffix(MSG_END)
}