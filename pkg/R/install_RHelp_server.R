#' @export
install_RHelp_server <- function(){
    RHelp_module_dir <- system.file("RHelp",package="RKernel")
    RHelp_install_call <- c("pip","install",RHelp_module_dir)
    exit_code <- system2(RHelp_install_call)
    invisible(exit_code)
}
