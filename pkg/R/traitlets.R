trait <- function(x,sync=FALSE,...) 
    structure(x,sync=sync,class=c("trait",class(x)),...)

# structure(as.expression(substitute(x)),sync=sync,class=c("trait",class(x)),...)



is.trait <- function(x) inherits(x,"trait")

str_enum <- function(x,type="str_unum",...) trait(x,type=type,...)
