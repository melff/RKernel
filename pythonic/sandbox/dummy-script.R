r <- 0L
for(i in 1:4){
    cat(sprintf("%d. ",i))
    cat("Hello World!\n")
    r <- r + 1L
}


fun <- function(){
    for(i in 1:4){
        text <- paste0(sprintf("%d. ",i),"Hello World!")
        print(text)
        r <- r + 1L
    }
}


fun()

