.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is rioja ",
    utils::packageDescription("rioja", field="Version"), appendLF = TRUE)
}

.onUnload <- function(libpath)
    library.dynam.unload("rioja", libpath)


