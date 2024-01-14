r <- 0L
for(i in 1:4){
    cat(sprintf("%d. ",i))
    cat("Hello World!")
    Sys.sleep(0.15)
    r <- r + 1L
}


fun <- function(){
    for(i in 1:4){
        print(sprintf("%d. ",i))
        print("Hello World!")
        r <- r + 1L
    }
}


fun()
