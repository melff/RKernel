#' install the R Kernel
#'
#' @export
install <- function(){
    installspec()
    install_RHelp_server()
}


#' @rdname install
#' Install the R Kernel spec
#'
#' @importFrom jsonlite fromJSON toJSON
#' @export
# The following is adapted from IRKernel/R/installspec.r
installspec <- function(user=TRUE,prefix=NULL){
   kernelspec_srcdir <- system.file("kernelspec",package="RKernel")
   tmp_dir <- tempfile()
   dir.create(tmp_dir)
   file.copy(kernelspec_srcdir,tmp_dir,recursive=TRUE)
   
   json_infile <- file.path(tmp_dir,"kernelspec","RKernel.json")
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
       "--name","rkernel",
       if(user) "--user" else NULL,
       if(length(prefix)) paste0("--prefix=",prefix),
       file.path(tmp_dir,"kernelspec")
   )
   exit_code <- system2(jupyter_call)

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

   unlink(tmp_dir,recursive=TRUE)
   invisible(exit_code)
}

#' @rdname install
#' Install an RHelp server module
#'
#' @description Install a Python module that provides for an R process being
#'     started when needed to display R help pages.
#' @export
install_RHelp_server <- function(){
    RHelp_module_dir <- system.file("RHelp",package="RKernel")
    RHelp_install_call <- paste("python3","-m pip","install",RHelp_module_dir)
    exit_code <- system(RHelp_install_call)
    invisible(exit_code)
}
