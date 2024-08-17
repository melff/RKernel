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

json_send <- function(msg){
    # log_out("send_msg\n")
    # log_out(msg,use.print=TRUE)
    msg <- to_json(msg)
    # log_out(msg)
    cat_(DLE,JSON_BEGIN,msg,JSON_END,DLE,sep="")
    error=function(e) log_error(conditionMessage(e))
}
msg_send <- json_send

json_unwrap <- fromJSON
msg_unwrap <- json_unwrap  