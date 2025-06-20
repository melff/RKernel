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
#' @param kernel_name String; the name of the kernel. To be used 
#'      e.g. as the '--kernel=' argument for the CLI call 'jupyter console'
#' @param display_name String; the name as it appears in the Jupyter
#'      web interface
#' @param env A list or NULL; optional environmental variables
#'      for the R kernel process. For example, to get single-threaded 
#'      open-blas use 
#'      "\code{env = list(OPENBLAS_NUM_THREADS = 1)}".
#'      To force a single OMP thread use
#'      "\code{env = list(OMP_NUM_THREADS = 1)}"
#' @export
# The following is adapted from IRKernel/R/installspec.r
installspec <- function(user = TRUE,
                        prefix = NULL,
                        kernel_name = "rkernel",
                        display_name = "R",
                        env = NULL){
   kernelspec_srcdir <- system.file("kernelspec",package="RKernel")
   tmp_dir <- tempfile()
   dir.create(tmp_dir)
   file.copy(kernelspec_srcdir,tmp_dir,recursive=TRUE)
   
   json_outfile <- file.path(tmp_dir,"kernelspec","kernel.json")
   # Put the absolute path of the current interpreter
   kernelspec$argv[[1]] <- file.path(R.home("bin"),"R")
   kernelspec$display_name = display_name
   write_json(kernelspec,pretty=TRUE,auto_unbox=TRUE,
         path=json_outfile)
   jupyter_call <- c(
       "jupyter",
       "kernelspec",
       "install",
       "--replace",
       "--name", kernel_name,
       if(user) "--user" else NULL,
       if(length(prefix)) paste0("--prefix=",prefix),
       file.path(tmp_dir,"kernelspec")
   )
   exit_code <- system2(jupyter_call)

   unlink(tmp_dir,recursive=TRUE)
   invisible(exit_code)
}

kernelspec <- list(
    argv = list(
        "R",
        "--slave",
        "--vanilla", 
        "-e", 
        "RKernel::main()", 
        "--args", 
        "{connection_file}"
    ),
    language = "R",
    metadata = list(
        debugger = FALSE
    )
)