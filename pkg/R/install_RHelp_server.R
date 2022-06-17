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
