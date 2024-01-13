r <- 0L
#browser()
for(i in 1:4){
    cat(sprintf("%d. ",i))
    cat("Hello World!\n")
    r <- r + 1L
}
#stop("Oopsie!")
fun <- function(){
    for(i in 1:4){
        print(sprintf("%d. ",i))
        print("Hello World!")
        r <- r + 1L
    }
}

#debug(fun)

fun()
