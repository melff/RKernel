#' install the R Kernel
#'
#' @export
install <- function(){
    installspec()
}


#' @describeIn install Install the R Kernel spec
#'
#' @importFrom jsonlite fromJSON toJSON read_json write_json
#' @param user Logical, whether to install the kernel in the user's home directory
#' @param prefix NULL or a character string with a path prefix
#' @param single_blas Logical, whether the kernel should use a single-threaded BLAS library.
#' @export
# The following is adapted from IRKernel/R/installspec.r
installspec <- function(user=TRUE,prefix=NULL,single_blas=FALSE){
   kernelspec_srcdir <- system.file("kernelspec",package="RKernel")
   tmp_dir <- tempfile()
   dir.create(tmp_dir)
   file.copy(kernelspec_srcdir,tmp_dir,recursive=TRUE)
   
   json_infile <- file.path(tmp_dir,"kernelspec","RKernel.json")
   json_outfile <- file.path(tmp_dir,"kernelspec","kernel.json")
   kernelspec <- read_json(json_infile)
   # Put the absolute path of the current interpreter
   kernelspec$argv[[1]] <- file.path(R.home("bin"),"R")
   write_json(kernelspec,pretty=TRUE,auto_unbox=TRUE,
         path=json_outfile)
   unlink(json_infile)
   jupyter_call <- c(
       "jupyter",
       "kernelspec",
       "install",
       "--replace",
       "--name","rkernel",
       if(user) "--user" else NULL,
       if(length(prefix)) paste0("--prefix=",prefix),
       file.path(tmp_dir,"kernelspec")
   )
   exit_code <- system2(jupyter_call)

   if(single_blas){ 
       json_infile <- file.path(tmp_dir,"kernelspec","RKernel1blas.json")
       json_outfile <- file.path(tmp_dir,"kernelspec","kernel.json")
       kernelspec <- fromJSON(json_infile)
       # Put the absolute path of the current interpreter
       kernelspec$argv[[1]] <- file.path(R.home("bin"),"R")
       write(toJSON(kernelspec,pretty=TRUE,auto_unbox=TRUE),
             file=json_outfile)
       unlink(json_infile)
       jupyter_call <- c(
           "jupyter",
           "kernelspec",
           "install",
           "--replace",
           "--name","rkernel1blas",
           if(user) "--user" else NULL,
           if(length(prefix)) paste0("--prefix=",prefix),
           file.path(tmp_dir,"kernelspec")
       )
       exit_code <- system2(jupyter_call)
   }
   unlink(tmp_dir,recursive=TRUE)
   invisible(exit_code)
}
