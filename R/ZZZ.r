#.First.lib <- function(libname, pkgname)
#{
#    library.dynam("rioja", pkgname, libname)
#}

.onLoad <- function(libname, pkgname)
{
    library.dynam("rioja", pkgname, libname)
}

.onUnload <- function(libpath)
    library.dynam.unload("rioja", libpath)
