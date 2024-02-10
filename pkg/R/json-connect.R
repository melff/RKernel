EOT <- '\x04'
DLE <- '\x10'
ETB <- '\x17'
JSON_MSG <- '[!JSON]'
MSG_PROMPT <- 'MSG?'

json_send <- function(msg){
    # log_out("send_msg\n")
    # log_out(msg,use.print=TRUE)
    msg <- to_json(msg)
    # log_out(msg)
    cat_(DLE,JSON_MSG,msg,ETB,DLE,sep="")
    error=function(e) log_error(conditionMessage(e))
}
