#' @importFrom jsonlite fromJSON toJSON

# The following is adapted from IRKernel/R/installspec.r
#' @export
installspec <- function(){
   kernelspec_srcdir <- system.file("kernelspec",package="RKernel")
   tmp_dir <- tempfile()
   dir.create(tmp_dir)
   file.copy(kernelspec_srcdir,tmp_dir,recursive=TRUE)
   json_tmpfile <- file.path(tmp_dir,"kernelspec","kernel.json")
   kernelspec <- fromJSON(json_tmpfile)
   # Put the absolute path of the current interpreter
   kernelspec$argv[[1]] <- file.path(R.home("bin"),"R")
   write(toJSON(kernelspec,pretty=TRUE,auto_unbox=TRUE),
         file=json_tmpfile)
   jupyter_call <- c(
       "jupyter",
       "kernelspec",
       "install",
       "--replace",
       "--name","rkernel",
       "--user",
       file.path(tmp_dir,"kernelspec")
   )
   exit_code <- system2(jupyter_call)
   unlink(tmp_dir,recursive=TRUE)
   invisible(exit_code)
}
