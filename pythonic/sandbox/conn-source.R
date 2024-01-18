source_port <- function(port){
    con <- try(socketConnection(port = port,server=TRUE),
               silent=TRUE)
    if(inherits(con,"connection")){
        source(con,echo=TRUE)
        close(con)
    }
}

source_port(port=26011)
