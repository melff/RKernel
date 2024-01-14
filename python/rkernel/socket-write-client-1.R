send <- function(port,text){
    con <- try(socketConnection(port = port,blocking=TRUE),
                    silent=TRUE)
    if(inherits(con1,"connection")){
        writeChar(text,con)
        close(con)
        return(TRUE)
    }
    else return(FALSE)
}

for(i in 1:4){
    send(text=format(Sys.time()),port=26011)
}
message("Done!")
