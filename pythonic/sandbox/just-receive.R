
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

r <- receive(port=26011)
print(r)
