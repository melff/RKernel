send <- function(port,text){
    con <- try(socketConnection(port = port,blocking=TRUE),
                    silent=TRUE)
    if(inherits(con,"connection")){
        writeChar(text,con)
        close(con)
        return(TRUE)
    }
    else return(FALSE)
}


receive <- function(port){
    con <- try(socketConnection(port = port, server=TRUE),
                    silent=TRUE)
    if(inherits(con,"connection")){
        response <- readLines(con)
        close(con)
        return(response)
    }
    else return(NULL)
}

send(port=26011,text="Ping")

r <- receive(port=26012)
print(r)
