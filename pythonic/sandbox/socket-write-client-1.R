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
    con <- try(socketConnection(port = port,blocking=TRUE),
                    silent=TRUE)
    if(inherits(con,"connection")){
        response <- readLines(con)
        close(con)
        return(response)
    }
    else return(NULL)
}

send_and_receive <- function(text,port){
    repeat {
        con <- try(socketConnection(port = port,blocking=FALSE),
                   silent=TRUE)
        if(inherits(con,"connection")) break
    }
    cat("Connection created ...\n")
    cat("Sending message ...\n")
    writeChar(text,con)
    cat("Waiting for response ...\n")
    repeat{
        response <- readLines(con)
        if(length(response) > 0) break
    }
    close(con)
    return(response)
}

for(i in 1:4){
    message <- format(Sys.time())
    cat(sprintf("Sending message '%s'\n",message))
    response <- send_and_receive(text=message,port=26011)
    print(response)
}
message("Done!")
